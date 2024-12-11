{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module SupportFunctionsSpec where

import ActionSpaces
import ArbitraryInstances
import Parameterization
import Types
import SupportFunctions
import TimeHelperFunctions

import qualified Data.Map.Strict as M
import Test.Hspec
import Test.QuickCheck

{---------------------------
Tests of basic functionality
----------------------------}

---------------
-- 1. Run Tests
---------------

spec :: Spec
spec = do
  describe "Verify account logic" $ do
    it "All random accounts have only positive entries"  $ property $
      prop_TestPositiveAccountGen
  describe "Time specific tests" $ do
    it "verify conditions of dynamic time lock" $ property $
      prop_DynamicTimeLockDuration
  describe "stake unstake function" $ do
    it "verify that unstaking works as expected" $ property $
      prop_StakeOrUnstakeUnstake
    it "verify that unstaking fails silently when insufficient funds" $ property $
      prop_StakeOrUnstakeUnstakeInsuff
    it "verify that staking works as expected" $ property $
      prop_StakeOrUnstakeStake
    it "verify that staking fails silently when insufficient funds" $ property $
      prop_StakeOrUnstakeStakeInsuff
  describe "heck the utility function computes the right values" $ do
    it "check negative values" $ property $
      prop_computeAssetsAtRiskNeg
    it "check in case of missing name" $ property $
      prop_computeAssetsMissingName

--------------------------------------------
-- 2. Verify properties of various functions
--------------------------------------------

-- Test that all the entries in accounts are positive
prop_TestPositiveAccountGen ::  Property
prop_TestPositiveAccountGen  =
  forAll genAccount $ \account ->
  all (> 0) $ M.elems account


----------------------
-- 3. Helper functions
----------------------

prop_DynamicTimeLockDuration params currentRageQuitSupport
  | (currentRageQuitSupport <= firstSealRageQuitSupport params)
      = dynamicTimelockDuration params currentRageQuitSupport == 0
  | (firstSealRageQuitSupport params < currentRageQuitSupport
    && currentRageQuitSupport < secondSealRageQuitSupport params)
      = dynamicTimelockDuration params currentRageQuitSupport ==
          dynamicTimelockMinDuration params
      + (scaleTimeRelative ((currentRageQuitSupport - firstSealRageQuitSupport params)
                            / (secondSealRageQuitSupport params - firstSealRageQuitSupport params))
                            (dynamicTimelockMaxDuration params - dynamicTimelockMinDuration params))
  | (currentRageQuitSupport >= secondSealRageQuitSupport params)
      = dynamicTimelockDuration params currentRageQuitSupport ==
        dynamicTimelockMaxDuration params


---------------------------------
-- 4. Stake/Unstake functionality
---------------------------------

-- Update the map by adding `amount` to the value at `key`, or insert `amount` if the key is not present.
adjustOrInsert :: (Ord k, Num a) => k -> a -> M.Map k a -> M.Map k a
adjustOrInsert key amount = M.insertWith (+) key amount

-- Unstake from an account with sufficient funds
prop_StakeOrUnstakeUnstake  agent amount globalLidoState signallingEscrowState =
  amount < 0 ==>
  let accountStETH' = accountsStETH globalLidoState
      signallingEscrowState' = signallingEscrowState {lockedStETH = M.fromList[(agent,-amount + 1)]} -- ^ ensure that account has sufficient amounts available
      lockedStETH'  = lockedStETH signallingEscrowState'
      expectedGlobalLidoState = globalLidoState {accountsStETH = adjustOrInsert agent (-amount) accountStETH' }
      expectedSignallingEscrowState = signallingEscrowState' {lockedStETH = adjustOrInsert agent amount lockedStETH'}
      in stakeOrUnstake agent amount globalLidoState signallingEscrowState' === (expectedGlobalLidoState,expectedSignallingEscrowState)

-- Unstake from an account with insufficient funds
prop_StakeOrUnstakeUnstakeInsuff agent amount globalLidoState signallingEscrowState =
  amount < 0 ==>
  let signallingEscrowState' = signallingEscrowState {lockedStETH = M.fromList[(agent,-amount - 1)]} 
      in stakeOrUnstake agent amount globalLidoState signallingEscrowState' === (globalLidoState,signallingEscrowState')

-- Stake from an account with sufficient funds
prop_StakeOrUnstakeStake agent amount globalLidoState signallingEscrowState =
  amount > 0 ==>
  let lockedStETH'  = lockedStETH signallingEscrowState
      globalLidoState' = globalLidoState {accountsStETH = M.fromList[(agent,amount + 1)]} -- ^ ensure that account has sufficient amounts available
      accountStETH' = accountsStETH globalLidoState'
      expectedGlobalLidoState = globalLidoState' {accountsStETH = adjustOrInsert agent (-amount) accountStETH' }
      expectedSignallingEscrowState = signallingEscrowState {lockedStETH = adjustOrInsert agent amount lockedStETH'}
      in stakeOrUnstake agent amount globalLidoState' signallingEscrowState === (expectedGlobalLidoState,expectedSignallingEscrowState)

-- Stake from an account with insufficient funds
prop_StakeOrUnstakeStakeInsuff agent amount globalLidoState signallingEscrowState =
  amount < 0 ==>
  let globalLidoState' = globalLidoState {accountsStETH = M.fromList[(agent,amount - 1)]} 
      in stakeOrUnstake agent amount globalLidoState' signallingEscrowState === (globalLidoState',signallingEscrowState)

----------------------
-- Utility computation
----------------------

-- Check the utility function computes the right values
prop_computeAssetsAtRiskNeg globalLidoState riskFactorPublic =
   riskFactorPublic < 0  ==>
     let agent = "StakingAgent"
         globalLidoState' = globalLidoState {accountsStETH = M.fromList[(agent,100)]}
         in computeAssetsAtRisk agent (globalLidoState', riskFactorPublic) 1.0  === 100*riskFactorPublic

-- Check the utility function computes the right values - missing name
prop_computeAssetsMissingName agent globalLidoState riskFactorPublic =
   riskFactorPublic < 0  ==>
     let agentFixed = "StakingAgent"
         globalLidoState' = globalLidoState {accountsStETH = M.fromList[(agentFixed,100)]}
         in computeAssetsAtRisk agent (globalLidoState', riskFactorPublic) 1.0 === 0
