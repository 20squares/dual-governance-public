{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module DualGovernanceMechanismSpec where

import ActionSpaces
import ArbitraryInstances
import Parameterization
import Types
import SupportFunctions
import TimeHelperFunctions

import Debug.Trace
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Time (UTCTime(..), addUTCTime)
import Test.Hspec
import Test.QuickCheck

{------------------------------------------------------------
Test regarding the mechanics of the dual governance mechanism
-------------------------------------------------------------}

---------------
-- 1. Run Tests
---------------

spec :: Spec
spec = do
  describe "Normal to VetoSignalling" $ do
    it "transitions to Veto-Signalling if R > R_1"  $ property $
      prop_TransitionFromNormal
  describe "transition from VetoSignalling" $ do
    it "basic" $ property $
      prop_TransitionFromVetoSignalling
    it "to RageQuit when time increases" $ property $
      prop_TransitionVetoSignallingToRageQWithTimeIncrease
    it "to RageQuit when time increases - random conditions" $ property $
      prop_TransitionVetoSignallingToRageQWithTimeIncrease2
    it "to Deactivation when time increases" $ property $
      prop_TransitionVetoSignallingToDeactWithTimeIncrease
  describe "transition from VetoSignallingDeactivation" $ do
    it "basic" $ property $
      prop_TransitionFromDeactivation
    it "to VetoSignalling" $ property $
      prop_TransitionFromDeactivationToVetoSignalling
    it "to RageQuit" $ property $ 
      prop_TransitionFromDeactivationToRageQuit
    it "to Cooldown" $ property $
      prop_TransitionFromDeactivationToCooldown
    it "prevent coming from VetoDeactivation to Deactivation with new values" $ property $
      prop_PreventReenteringDeact
  describe "transition from VetoCooldown" $ do
    it "basic" $ property $
      prop_TransitionFromVetoCooldown
    it "to VetoSignalling" $ property $
      prop_TransitionFromCooldownToVetoSignalling
    it "transition from RageQuit" $ property $
      prop_TransitionFromRageQuit
    it "Test whether the time reset conditions are actually correct" $ property $
      prop_NoActivationResetReenteringVetoSignalling
  describe "Many transitions" $ do
    it "Transition from Normal to VetoSignalling" $ property $
      prop_transition1
    it "Transition one state from VetoSignalling to Ragequit" $ property $
      prop_transition12
    it "Transition from from VetoSignalling to Rage-Quit ignoring time." $ property $
      prop_transitionMany


-----------------------
-- 2. State transitions
-----------------------

-- From normal state to veto signalling
prop_TransitionFromNormal :: UTCTime -> GovernanceValues -> GovernanceParams -> Property
prop_TransitionFromNormal currentTime values params =
  -- Ensure both r_1 and r are positive
  forAll (arbitrary `suchThat` (> 0)) $ \r ->
    if r > firstSealRageQuitSupport params
    then transitionNormal  params currentTime r values
         == Just (VetoSignalling, values {timeOfActivation = currentTime})
    else transitionNormal params currentTime r values
         == Nothing

-- From veto signalling to new state
prop_TransitionFromVetoSignalling params currentTime currentRageQuitSupport values
   | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ VetoSignalling -> RageQuit
     > dynamicTimelockMaxDuration params
  && currentRageQuitSupport > secondSealRageQuitSupport params)
      = transitionVetoSignalling params currentTime currentRageQuitSupport values ==
       Just (RageQuit, values {
            timeOfActivation = currentTime
          , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
          , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
        })
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ VetoSignalling -> Deactivation
     > dynamicTimelockDuration params currentRageQuitSupport
  && timeDifferenceAbsolute currentTime (max (timeOfActivation values) (timeOfReactivation values))
     > vetoSignallingMinActiveDuration params)
      = transitionVetoSignalling params currentTime currentRageQuitSupport values ==
        Just (VetoSignallingDeactivation, values {timeOfDeactivation = currentTime})
  | otherwise = transitionVetoSignalling params currentTime currentRageQuitSupport values ==
        Nothing

-- Verify that if time increases a state transition will happen to RageQuit (we assume other conditions are met)
prop_TransitionVetoSignallingToRageQWithTimeIncrease :: TimeAbsolute -> Property 
prop_TransitionVetoSignallingToRageQWithTimeIncrease startTime =
  -- Generate an increasing sequence of times
  forAll (arbitrary `suchThat` (> 0) :: Gen TimeRelative) $ \daysToAdd ->
    let currentTime = addUTCTime (daysToAdd * days) startTime
        -- Perform the initial state transition with the starting time
        initialTransition = transitionVetoSignalling params startTime currentRageQuitSupport values
        -- Perform the updated transition with the increased time
        newTransition = transitionVetoSignalling params currentTime currentRageQuitSupport values
        in (newTransition 
            == Just (RageQuit, values {
                    timeOfActivation = currentTime
                  , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
                  , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
                }))
            &&
            (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    currentRageQuitSupport = 0.2
    values = defaultGovernanceValues

-- Verify that if time increases a state transition will happen to RageQuit (we assume other conditions are met)
-- NOTE this test works explicitly
prop_TransitionVetoSignallingToRageQWithTimeIncrease2 startTime governanceParams values =
  -- Generate an increasing sequence of times
  forAll (arbitrary `suchThat` (> 0) :: Gen TimeRelative) $ \daysToAdd ->
    let currentTime = addUTCTime (daysToAdd * days) startTime
        governanceParams' = governanceParams {secondSealRageQuitSupport = 0.0, firstSealRageQuitSupport = 0.0}
        -- Perform the initial state transition with the starting time
        initialTransition = transitionVetoSignalling governanceParams' startTime currentRageQuitSupport values
        -- Perform the updated transition with the increased time
        newTransition = transitionVetoSignalling governanceParams' currentTime currentRageQuitSupport values
        in (newTransition 
            === Just (RageQuit, values {
                    timeOfActivation = currentTime
                  , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
                  , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
                }))
            .&&.
            (newTransition =/= initialTransition)
  where
    currentRageQuitSupport = 0.2


-- Verify that if time increases a state transition will happen to Deactivation (we assume other conditions are met, and RageQuit conditions are not met)
prop_TransitionVetoSignallingToDeactWithTimeIncrease :: TimeAbsolute -> Property 
prop_TransitionVetoSignallingToDeactWithTimeIncrease startTime =
  -- Generate an increasing sequence of times
  forAll (arbitrary `suchThat` (> 0) :: Gen TimeRelative) $ \daysToAdd ->
    let currentTime = addUTCTime (daysToAdd * days) startTime
        -- The initial state transition
        initialTransition = transitionVetoSignalling params startTime currentRageQuitSupport values
        -- Perform the updated transition with the increased time
        newTransition = transitionVetoSignalling params currentTime currentRageQuitSupport values
        in (newTransition ==Just (VetoSignallingDeactivation, values {timeOfDeactivation = currentTime}))
           &&
           (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    currentRageQuitSupport = 0.1
    values = defaultGovernanceValues


-- From deactivation to new state
prop_TransitionFromDeactivation params currentTime currentRageQuitSupport values 
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ Deactivation -> VetoSignalling
      < dynamicTimelockDuration params currentRageQuitSupport)
    = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
      == Just (VetoSignalling, values {timeOfReactivation = currentTime}) -- do not change timeOfActivation
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ Deactivation -> RageQuit
     >= dynamicTimelockMaxDuration params
  && currentRageQuitSupport > secondSealRageQuitSupport params)
      = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
        == Just (RageQuit, values {
            timeOfActivation = currentTime
          , timeOfRageQuitExtensionStart = Nothing -- mark withdrawal NFTs as not yet claimed
          , rageQuitSeqNumber = rageQuitSeqNumber values + 1 -- increment sequence number
        })
  | (timeDifferenceAbsolute currentTime (timeOfDeactivation values) -- ^ Deactivation -> VetoCoolDown
      >= vetoSignallingDeactivationMaxDuration params)
    = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
      == Just (VetoCooldown, values {timeOfActivation = currentTime})
  | (otherwise)
    = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
      == Nothing

-- From deactivation back to VetoSignalling if currentRageQuitSupport increases from a fixed value
prop_TransitionFromDeactivationToVetoSignalling currentTime =
 forAll (arbitrary `suchThat` (> threshold) :: Gen Double) $ \currentRageQuitSupport ->
     let initialTransition = transitionVetoSignallingDeactivation params currentTime 0.001 values
         newTransition     = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
         in (newTransition
             == Just (VetoSignalling, values {timeOfReactivation = currentTime}))
            &&
            (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    currentRageQuitSupport = 0.1
    values = defaultGovernanceValues
    threshold = firstSealRageQuitSupport params

-- From deactivation to RageQuit if time and currentRageQuitSupport increases (from a fixed value)
prop_TransitionFromDeactivationToRageQuit startTime =
 forAll (arbitrary `suchThat` (> threshold) :: Gen Double) $ \currentRageQuitSupport ->
   forAll (arbitrary `suchThat` (> 0) :: Gen TimeRelative) $ \daysToAdd ->
     let  currentTime = addUTCTime (daysToAdd * days) startTime
          initialTransition = transitionVetoSignallingDeactivation params startTime 0.001 values
          newTransition     = transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
          in (newTransition
               == Just (RageQuit, values {
                  timeOfActivation = currentTime
                , timeOfRageQuitExtensionStart = Nothing 
                , rageQuitSeqNumber = rageQuitSeqNumber values + 1
              }))
             &&
             (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    values = defaultGovernanceValues
    threshold = secondSealRageQuitSupport params

-- From deactivation to Cooldown if time increases
-- NOTE keep currentRageQuitSupport small to avoid other transitions
prop_TransitionFromDeactivationToCooldown startTime =
  forAll (arbitrary `suchThat` (> threshold) :: Gen TimeRelative) $ \daysToAdd ->
     let  currentTime = addUTCTime (daysToAdd * days) startTime
          initialTransition = transitionVetoSignallingDeactivation params startTime 0.001 values
          newTransition     = transitionVetoSignallingDeactivation params currentTime 0.001 values
          in (newTransition
               == Just (VetoCooldown, values {timeOfActivation = currentTime}))
             &&
             (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    values = defaultGovernanceValues
    threshold = vetoSignallingDeactivationMaxDuration params / days

-- From VetoCooldown to new state
prop_TransitionFromVetoCooldown params currentTime currentRageQuitSupport values
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) >= vetoCooldownDuration params -- ^ VetoCoolDown -> VetoSignalling
  && currentRageQuitSupport > firstSealRageQuitSupport params)
    = transitionVetoCooldown params currentTime currentRageQuitSupport values
      == Just (VetoSignalling, values {timeOfActivation = currentTime})
  | (timeDifferenceAbsolute currentTime (timeOfActivation values) >= vetoCooldownDuration params -- ^ VetoCoolDown -> Normal
  && currentRageQuitSupport <= firstSealRageQuitSupport params)
    = transitionVetoCooldown params currentTime currentRageQuitSupport values
      == Just (Normal, values {timeOfActivation = currentTime})
  | (otherwise)
    = transitionVetoCooldown params currentTime currentRageQuitSupport values
      == Nothing

-- From Cooldown to VetoSingalling if time increases
-- NOTE We assume the second condition to be fulfilled
prop_TransitionFromCooldownToVetoSignalling startTime =
  forAll (arbitrary `suchThat` (> threshold) :: Gen TimeRelative) $ \daysToAdd ->
     let  currentTime = addUTCTime (daysToAdd * days) startTime
          initialTransition = transitionVetoCooldown params currentTime 0.001 values
          newTransition     = transitionVetoCooldown params currentTime currentRageQuitSupport values
          in (newTransition
               == Just (VetoSignalling, values {timeOfActivation = currentTime}))
             &&
             (newTransition /= initialTransition)
  where
    params = defaultGovernanceParams
    values = defaultGovernanceValues
    currentRageQuitSupport = firstSealRageQuitSupport params + 0.1
    threshold = vetoCooldownDuration params / days

-- From RageQuit to new state
prop_TransitionFromRageQuit params currentTime currentRageQuitSupport values 
  = case timeOfRageQuitExtensionStart values of {
        Nothing -- withdrawal NFTs have not yet been claimed
          -> transitionRageQuit params currentTime currentRageQuitSupport values
             == Nothing
      ; Just timeOfRageQuitExtensionStart'
          | (timeDifferenceAbsolute currentTime timeOfRageQuitExtensionStart' -- ^ RageQuit -> VetoSignalling
              >= rageQuitExtensionDelay params + ethWithdrawalTimelockDuration params (rageQuitSeqNumber values)
            && currentRageQuitSupport > firstSealRageQuitSupport params)
            -> transitionRageQuit params currentTime currentRageQuitSupport values
               == Just (VetoSignalling, values {timeOfActivation = currentTime})
          | (timeDifferenceAbsolute currentTime timeOfRageQuitExtensionStart' -- ^ RageQuit -> VetoCooldown
              >= rageQuitExtensionDelay params + ethWithdrawalTimelockDuration params (rageQuitSeqNumber values)
            && currentRageQuitSupport <= firstSealRageQuitSupport params)
            -> transitionRageQuit params currentTime currentRageQuitSupport values
               == Just (VetoCooldown, values {timeOfActivation = currentTime})
          | (otherwise)
            -> transitionRageQuit params currentTime currentRageQuitSupport values
               == Nothing
  }

-- Test whether one can advance from veto-signalling to deactivation, if in the deactivation phase
prop_PreventReenteringDeact globalLidoState params currentTime signallingEscrowState  values =
  transition1 globalLidoState params currentTime signallingEscrowState VetoSignallingDeactivation values /= Just (VetoSignallingDeactivation, values)

prop_NoActivationResetReenteringVetoSignalling params currentTime currentRageQuitSupport values =
  let activationValue = Just $ timeOfActivation values
      actual          = fmap timeOfActivation $ fmap snd $ transitionVetoSignallingDeactivation params currentTime currentRageQuitSupport values
      notExpected     = Just currentTime
      in
        activationValue /= notExpected ==>
        (timeDifferenceAbsolute currentTime (timeOfActivation values) -- ^ Deactivation -> VetoSignalling
            <= dynamicTimelockDuration params currentRageQuitSupport
          || currentRageQuitSupport > secondSealRageQuitSupport params)
          ==> actual /= notExpected

-- Transition one state to VetoSignalling
prop_transition1 globalLidoState governanceParams currentTime signallingEscrowState  governanceValues =
    let governanceParams' = governanceParams {secondSealRageQuitSupport = 0.0, firstSealRageQuitSupport = 0.0}
        signallingEscrowState' = signallingEscrowState {lockedStETH = M.fromList [("name",100)]}
        Just (st,_) = transition1 globalLidoState governanceParams' currentTime signallingEscrowState' Normal governanceValues
        in st === VetoSignalling

-- Transition one state from VetoSignalling to Ragequit
-- NOTE we make current time sufficiently large for the transition to fail
prop_transition12 globalLidoState governanceParams  signallingEscrowState  governanceValues =
    forAll (arbitrary `suchThat` (> 0) :: Gen Integer) $ \t ->
        let currentTime = utcTimeFrom (10*t)
            governanceParams' = governanceParams {secondSealRageQuitSupport = 0.0, firstSealRageQuitSupport = 0.0}
            signallingEscrowState' = signallingEscrowState {lockedStETH = M.fromList [("name",100)]}
            Just (st,_) = transition1 globalLidoState governanceParams' currentTime signallingEscrowState' VetoSignalling governanceValues
            in st === RageQuit

-- Transition from VetoSignalling to Rage-Quit ignoring time.
-- This test has to fail in this form.
prop_transitionMany globalLidoState governanceParams  signallingEscrowState governanceValues  =
  forAll (arbitrary `suchThat` (> 0) :: Gen Integer) $ \t ->
    let currentTime = utcTimeFrom (10*t)
        governanceParams' = governanceParams {secondSealRageQuitSupport = 0.0, firstSealRageQuitSupport = 0.0}
        signallingEscrowState' = signallingEscrowState {lockedStETH = M.fromList [("name",100)]}
        (st,_) = transitionMany globalLidoState governanceParams' currentTime signallingEscrowState' VetoSignalling governanceValues
        in st === RageQuit
