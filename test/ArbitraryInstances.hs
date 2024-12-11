{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ArbitraryInstances where

import Types
import TimeHelperFunctions

import           Data.List (nub)
import qualified Data.Map as M
import Data.Time.Clock.TAI (AbsoluteTime, taiEpoch, addAbsoluteTime)
import Data.Time (UTCTime(..), Day, fromGregorian, addUTCTime, secondsToDiffTime, addUTCTime)
import Test.QuickCheck

{-
Collect all the relevant Arbitrary instances needed for tests
-}

-------------------------
-- 1. Arbitrary instances
-------------------------

-- Arbitrary instance for accounts 

-- Predefined list of agent names
predefinedAgents = ["Alice", "Bob", "Charlie", "Dave", "Eve"]

-- Custom generator for Account Double with fixed agent names and positive balances
genAccount :: Gen (Account Double)
genAccount = do
    -- Generate a list of unique agent names chosen from predefinedAgents
    agents <- nub <$> listOf (elements predefinedAgents)
    -- Generate corresponding positive balances for each agent
    balances <- vectorOf (length agents) (arbitrary `suchThat` (> 0))
    -- Combine them into an account (Map)
    return $ M.fromList (zip agents balances)
    
-- Arbitrary instance for GlobalLidoState
instance Arbitrary GlobalLidoState where
  arbitrary = do
    totalStETHSupply <- arbitrary `suchThat` (> 0)  -- Generate random StETH
    conversionRate   <- arbitrary `suchThat` (> 0)  -- Generate random ConversionRate
    accountsStETH    <- genAccount 
    accountsWstETH   <- genAccount
    return GlobalLidoState
      { totalStETHSupply = totalStETHSupply
      , conversionRate   = conversionRate
      , accountsStETH    = accountsStETH 
      , accountsWstETH   = accountsWstETH
      }

-- Arbitrary instance for SignallingEscrowState
instance Arbitrary SignallingEscrowState where
  arbitrary = do
    lockedStETH         <- genAccount -- Generate random StETH
    lockedWstETH        <- genAccount -- Generate random WstETH
    unfinalizedNFTTotal <- genAccount -- Generate random unfinalized StETH
    finalizedNFTTotal   <- genAccount -- Generate random ETH
    return SignallingEscrowState
      { lockedStETH = lockedStETH
      , lockedWstETH = lockedWstETH
      , unfinalizedNFTTotal = unfinalizedNFTTotal
      , finalizedNFTTotal = finalizedNFTTotal
      }

-- Arbitrary instance for GovernanceParams
instance Arbitrary GovernanceParams where
  arbitrary = do
    proposalExecutionMinTimelock <- arbitrary
    firstSealRageQuitSupport <- arbitrary `suchThat` (\x -> x >= 0 && x <= 1) -- Assume between 0 and 1
    secondSealRageQuitSupport <- arbitrary `suchThat` (\x -> x >= 0 && x <= 1)
    dynamicTimelockMinDuration <- arbitrary `suchThat` (\x -> x > days)
    dynamicTimelockMaxDuration <- arbitrary `suchThat` (\x -> x >= dynamicTimelockMinDuration)
    vetoSignallingMinActiveDuration <- arbitrary `suchThat` (\x -> x >= 0)
    vetoSignallingDeactivationMaxDuration <- arbitrary `suchThat` (\x -> x >= 0)
    vetoCooldownDuration <- arbitrary `suchThat` (\x -> x >= 0)
    rageQuitExtensionDelay <- arbitrary `suchThat` (\x -> x >= 0)
    rageQuitEthWithdrawalsMinTimelock <- arbitrary `suchThat` (\x -> x >= 0)
    rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber <- arbitrary `suchThat` (> 0) -- Positive integers
    rageQuitEthWithdrawalsTimelockGrowthCoeffs <- (,,)
      <$> arbitrary `suchThat` (\x -> x >= 0) -- First coefficient
      <*> arbitrary `suchThat` (\x -> x >= 0) -- Second coefficient
      <*> arbitrary `suchThat` (\x -> x >= 0) -- Third coefficient
    
    return GovernanceParams
      { proposalExecutionMinTimelock = proposalExecutionMinTimelock
      , firstSealRageQuitSupport = firstSealRageQuitSupport
      , secondSealRageQuitSupport = secondSealRageQuitSupport
      , dynamicTimelockMinDuration = dynamicTimelockMinDuration
      , dynamicTimelockMaxDuration = dynamicTimelockMaxDuration
      , vetoSignallingMinActiveDuration = vetoSignallingMinActiveDuration
      , vetoSignallingDeactivationMaxDuration = vetoSignallingDeactivationMaxDuration
      , vetoCooldownDuration = vetoCooldownDuration
      , rageQuitExtensionDelay = rageQuitExtensionDelay
      , rageQuitEthWithdrawalsMinTimelock = rageQuitEthWithdrawalsMinTimelock
      , rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber = rageQuitEthWithdrawalsTimelockGrowthStartSeqNumber
      , rageQuitEthWithdrawalsTimelockGrowthCoeffs = rageQuitEthWithdrawalsTimelockGrowthCoeffs
      }

-- Arbitrary instance for GovernanceValues
instance Arbitrary GovernanceValues where
  arbitrary = do
    -- Start by generating a random base time for timeOfActivation
    timeOfActivation <- arbitrary
    -- Ensure timeOfDeactivation is after timeOfActivation by adding random days
    daysUntilDeactivation <- arbitrary `suchThat` (> 0)
    let timeOfDeactivation = addUTCTime (daysToNominalDiffTime daysUntilDeactivation) timeOfActivation
    -- Ensure timeOfReactivation is after timeOfDeactivation by adding more random days
    daysUntilReactivation <- arbitrary `suchThat` (> 0)
    let timeOfReactivation = addUTCTime (daysToNominalDiffTime daysUntilReactivation) timeOfDeactivation
    -- Optionally generate timeOfRageQuitExtensionStart
    timeOfRageQuitExtensionStart <- arbitrary
    rageQuitSeqNumber <- arbitrary `suchThat` (> 0)
    return GovernanceValues
      { timeOfActivation = timeOfActivation
      , timeOfReactivation = timeOfReactivation
      , timeOfDeactivation = timeOfDeactivation
      , timeOfRageQuitExtensionStart = timeOfRageQuitExtensionStart
      , rageQuitSeqNumber = rageQuitSeqNumber
      }

-- Arbitrary instance for TimeRelative (using days)
instance Arbitrary TimeRelative where
  arbitrary = do
    -- Generate random days within a reasonable range (up to 10 years)
    days <- arbitrary `suchThat` (\x -> x >= 0 && x < 10) -- Up to 10 days
    return $ daysToNominalDiffTime days

-- Arbitrary instance for AbsoluteTime (using days)
instance Arbitrary AbsoluteTime where
  arbitrary = do
    -- Generate a random number of days (within a reasonable range)
    days <- arbitrary `suchThat` (\x -> x >= 0 && x < 365 * 10) -- Up to 10 years worth of days
    -- Convert days to NominalDiffTime
    let nominalDays = daysToNominalDiffTime days
        diffTime = realToFrac nominalDays  -- Convert NominalDiffTime to DiffTime
    -- Add diffTime to the TAI epoch
    return $ addAbsoluteTime diffTime taiEpoch

-- Arbitrary instance for UTCTime
instance Arbitrary UTCTime where
  arbitrary = do
    -- Generate a random number of seconds (within a large range for the sake of example)
    days <- arbitrary `suchThat` (> 0) :: Gen Integer
    -- Define the UNIX epoch day (2024-09-01)
    let epochDay = fromGregorian 2024 9 1 :: Day
    -- Create a UTCTime from the epoch day and add random seconds
    return $ UTCTime epochDay (daysToDiffTime days)

-- Define a generator for risk values
genRisk :: Gen Double
genRisk = do
  agntRisk1 <- choose (0.01, 0.99)
  return agntRisk1
  