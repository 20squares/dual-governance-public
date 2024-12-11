{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GamesSpec where

import ActionSpaces
import Analytics
import ArbitraryInstances
import Parameterization
import ModelSupportFunctions
import Strategies
import SupportFunctions
import Types

import qualified Data.Map.Strict as M
import qualified Numeric.Probability.Distribution as N 
import Test.Hspec
import Test.QuickCheck

{----------------------------------
Test games with specific properties
-----------------------------------}

---------------
-- 1. Run Tests
---------------

spec :: Spec
spec = do
  describe "1. Verify staking game" $ do
    it "Test that staking behavior is in equilibrium - no staking"  $ property $
      prop_StakingGameEq
    it "Test that staking behavior is in equilibrium - staking"  $ property $
      prop_StakingGameEq2
    it "Test that state has changed and transfer was actually effective"  $ property $
      prop_StakingGameStateChange
  describe "ModelBasic - works as expected for undisputed proposal" $ do 
    it "Test that the proposal is in equilibrium"  $ property $
      prop_ModelBasicGameEq
    it "Test that the proposal has been executed"  $ property $
      prop_ModelBasicGameProposalExecuted
    it "Test that the gov state stays in normal"  $ property $
      prop_ModelBasicGameGovState
  describe "ModelBasic - works as expected for a disputed proposal" $ do
    it "Test that the proposal is in equilibrium"  $ property $
      prop_ModelBasicGameNegativeForStakersEq
    it "Test that the gov state changes to vetoSignalling"  $ property $
      prop_ModelBasicGameGovStateToVeto
  describe "ModelBasic2 - works as expected for undisputed proposal" $ do 
    it "Test that the proposal is in equilibrium"  $ property $
      prop_ModelBasic2GameEq
    it "Test that the state changes"  $ property $
      prop_ModelBasic2GameState
    it "Test that the proposal is pending" $ property $
      prop_ModelBasic2GameProposalPending
    it "Test that the gov state stays in normal"  $ property $
      prop_ModelBasic2GameGovState
  describe "ModelBasic2 - works as expected for a disputed proposal" $ do
    it "Test that the proposal is in equilibrium"  $ property $
      prop_ModelBasic2GameNegativeForStakersEq
    it "Show that cancel w/ pos staking is not an eq"  $ property $
      prop_ModelBasic2GameNegativeForStakersEqNoThreat
    it "Show that w/o threat of execution pos staking not an eq"  $ property $
      prop_ModelBasic2GameNegativeForStakersEqNoThreat2
    it "Test that the gov state changes to vetoSignalling"  $ property $
      prop_ModelBasic2GameGovStateToVeto
  describe "ModelBasic2 - test strategies" $ do
    it "Check staking strategy"  $ property $
      prop_ModelBasic2GameNegativeForMinStakersEq
  describe "ModelBasic2 - consider alternative continuations" $ do
    it "Consider the case where a veto-signalling leads to compromise"  $ property $
      prop_ModelBasic2ContinuationPos
  describe "ModelBasic2 - 2 stakers - Symmetric information between players" $ do
    it "Both players stake"  $ property $
      prop_ModelHeterogenousGameNegativeForStakersEq
    it "Only player stakes"  $ property $
      prop_ModelHeterogenousGameNegativeForStakersEq2
  describe "ModelHeterogenous - 2 stakers - Asymmetric information between players" $ do
    it "Players have the same subjective believes about the situation" $ property $
      prop_ModelHeterogenousAsymmetricInfoEq
    it "Players have different asymmetric valuations" $ property $
      prop_ModelHeterogenousAsymmetricInfoEq2
  describe "ModelBayesian - 2 stakers - Bayesian game - same beliefs" $ do
    it "with prob 1 very bad event happens" $ property $
      prop_ModelBayesianGameNegativeForStakersEq
    it "with prob 0 very bad event happens" $ property $
      prop_ModelBayesianGameNegativeForStakersEq2
    it "prob 0.5 bad and good event" $ property $
      prop_ModelBayesianGameNegativeForStakersEq3
    it "prob 0.5 bad and good event - unequal endowment" $ property $
      prop_ModelBayesianGameNegativeForStakersEq4
    it "prob 0.1 for negative event P1; prob 0.8 for negative event P2" $ property $
      prop_ModelBayesianGameNegativeForStakersEq5
    it "prob 0.1 for negative event P1; prob 0.8 for negative event P2; initial endowment unequally distributed" $ property $ 
      prop_ModelBayesianGameNegativeForStakersEq6
    it "prob 0.5 bad and good event; unequal endowment; only 1 staker NOT eq" $ property
      prop_ModelBayesianGameNegativeForStakersEq7
  describe "ModelBayesianEndogenousSignal - 2 stakers - uncertainty about effects" $ do
    it "both players perfect signals" $ property $
      prop_ModelBayesianEndogenousSignalGameNegativeForStakersEq
    it "both players total noise" $ property $
      prop_ModelBayesianEndogenousSignalGameNegativeForStakersEq2
  describe "ModelLeaderFollowerEndogenousSignal - 2 stakers - uncertainty about effects" $ do
    it "both players perfect signals" $ property $
      prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq
    it "Staker 1 noise, staker 2 noise" $ property $
      prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq2
    it "Staker 1 perfect signal, staker 2 noise" $ property $
      prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq3


------------------
-- 2. Staking game
------------------

-- With a given negative payoff from a proposal, not staking is the right choice as staking creates costs but does not prevent the proposal.
prop_StakingGameEq =
  eqEquilibriumStakingGame simpleStakingGameParameters (minStakingStrategyTuple stakerMoves) === True

-- With a given negative payoff from a proposal, minimal staking is the right choice as here the staking prevents the negative consequences
prop_StakingGameEq2 =
  eqEquilibriumStakingGame2 simpleStakingGameParameters (optimalStakingStrategyRageQuitTuple stakerMoves simpleStakingGameParameters) === True

-- Test whether one can advance from veto-signalling to deactivation, if  am in the deactivation phase
prop_StakingGameStateChange :: Property
prop_StakingGameStateChange = test simpleStakingGameParameters
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationStakingGame params (optimalStakingStrategyRageQuitTuple stakerMoves params) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          expected = N.decons result
          old      = [((signallingEscrowState,governanceState, governanceValues),1.0)]
          in expected =/= old


----------------
-- 3. ModelBasic
----------------

-- 3.1. Uncontested proposal => benefitting all sides

-- Test whether an uncontested proposal goes through
-- DAO proposes; no vetoing at staker level; execution of proposal
prop_ModelBasicGameEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  eqEquilibriumModelBasicGame params (modelBasicOptimalStrategyRageQuit stakerMoves params) === True
  where params = (modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders)

-- Test that the proposal has been executed
prop_ModelBasicGameProposalExecuted :: Double -> Double -> Property
prop_ModelBasicGameProposalExecuted payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  test (modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasicGame params (modelBasicOptimalStrategyRageQuit stakerMoves params) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,_,_, proposal'),_)] = N.decons result
          in (proposalState proposal') === Executed

-- Test that the governance state stays at normal
prop_ModelBasicGameGovState :: Double -> Double -> Property
prop_ModelBasicGameGovState payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  test (modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasicGame params (modelBasicOptimalStrategyRageQuit stakerMoves params) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,governanceState',_,_),_)] = N.decons result
          in governanceState' === governanceState

-- 3.2. Benefitting only LDO holders

-- Test whether a contested proposal goes through
prop_ModelBasicGameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasicGame params (modelBasicOptimalStrategyVetoSignalling stakerMoves params) === True
  where params = (modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders)
  
-- Test that the governance state changes to VetoSignalling
prop_ModelBasicGameGovStateToVeto :: Double -> Double -> Property
prop_ModelBasicGameGovStateToVeto payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  test (modelBasicGameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasicGame params (modelBasicOptimalStrategyRageQuit stakerMoves params) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,governanceState',_,_),_)] = N.decons result
          in governanceState' === VetoSignalling


-----------------
-- 4. ModelBasic2
-----------------

-- 4.1. Uncontested proposal => benefitting all sides

-- Test whether an uncontested proposal goes through with max staking strategy 
prop_ModelBasic2GameEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  eqEquilibriumModelBasic2Game params (modelBasic2ProposeExecuteMinStakingStrategy stakerMoves) === True
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)

-- Test that the overall state changes
prop_ModelBasic2GameState :: Double -> Double -> Property
prop_ModelBasic2GameState payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  test (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasic2Game params (modelBasic2ProposeCancelMaxStakingStrategy stakerMoves) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          expected = N.decons result
          old      = [((currentTime,signallingEscrowState,governanceState, governanceValues,proposal),1.0)]
          in expected =/= old

-- Test that the proposal is cancelled
prop_ModelBasic2GameProposalPending :: Double -> Double -> Property
prop_ModelBasic2GameProposalPending payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  test (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasic2Game params (modelBasic2ProposeCancelMaxStakingStrategy stakerMoves) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,_,_,proposal'),_)] = N.decons result
          in (proposalState proposal') === Cancelled

-- Test that the governance state stays in Normal 
prop_ModelBasic2GameGovState :: Double -> Double -> Property
prop_ModelBasic2GameGovState payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders > 0 ==>
  test (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasic2Game params (modelBasic2ProposeCancelMaxStakingStrategy stakerMoves) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,governanceState', _,_),_)] = N.decons result
          in governanceState' === Normal

-- 4.2. Benefitting only LDO holders

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
prop_ModelBasic2GameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasic2Game params (modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params) === True
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)

-- Shows that the threat for execution is relevant and needed
-- NOTE in this case min staking is optimal; but then so is execution of the proposal
prop_ModelBasic2GameNegativeForStakersEqNoThreat payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasic2Game params (modelBasic2ProposeCancelMinStakingStrategy stakerMoves) === False
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)

-- Shows that with cancel; pos staking is not an equilibrium; the eq fails because of staking
-- NOTE in this case min staking is optimal; but then so is execution of the proposal
prop_ModelBasic2GameNegativeForStakersEqNoThreat2 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasic2Game params (modelBasic2ProposeCancelOptimalVetoStakingStrategy stakerMoves params) === False
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)
 
-- Test that the governance state changes to VetoSignalling
prop_ModelBasic2GameGovStateToVeto :: Double -> Double -> Property
prop_ModelBasic2GameGovStateToVeto payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  test (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)
  where
    test params@GameParameters{..} =
      let result   = nextStateSimulationModelBasic2Game params (modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params) (globalLidoState, currentTime, signallingEscrowState, governanceState, governanceValues,proposal, opportunityCosts, agentRiskFactor)
          [((_,_,governanceState', _,_),_)] = N.decons result
          in governanceState' === VetoSignalling

-- 4.3 Exploring the strategy of players
prop_ModelBasic2GameNegativeForMinStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasic2Game params (modelBasic2ProposeExecuteOptimalMinStakingStrategy stakerMoves params) === True
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)

-- 4.4 Considering how future negotiations between dao and stETH holders evolves
-- Consider the case where a veto-signalling leads to compromise
prop_ModelBasic2ContinuationPos payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBasic2GameContinuation params (modelBasic2ProposeCancelMinStakingStrategy stakerMoves) (simulateContinuationPosProposal proposal2) === False
  where params = (modelBasic2GameParametersTestProposal payoffLDO payoffStETHHolders)

-----------------------------------
-- 5. ModelHeterogenous (2 stakers)
-----------------------------------

-- 5.1 Benefitting only LDO holders - symmetric public and private effect on stakers

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game both agents stake
prop_ModelHeterogenousGameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelHeterogenousGame params (modelHeterogenousProposeExecuteOptimalMinStakingStrategy stakerMoves params) === True
  where params = (modelHeterogenousGameParametersTestProposal payoffLDO payoffStETHHolders)

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game one agent stakes alone
prop_ModelHeterogenousGameNegativeForStakersEq2 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelHeterogenousGame params (modelHeterogenousProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) === True
  where params = (modelHeterogenousGameParametersTestProposal payoffLDO payoffStETHHolders)

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- NOTE In this game, both agents perceive zero risk and stake nothing accordingly
prop_ModelHeterogenousAsymmetricInfoEq payoffLDO payoffStETHHolders =
   payoffLDO > 0 && payoffStETHHolders < 0 ==>
   let params = (modelHeterogenousGameParametersTestProposalAsymmetric payoffLDO payoffStETHHolders 0 0)
     in eqEquilibriumModelHeterogenousGame params (modelHeterogenousProposeExecuteMinStakingStrategy stakerMoves) === True

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game one agent stakes alone
prop_ModelHeterogenousAsymmetricInfoEq2 payoffLDO payoffStETHHolders =
    forAll genRisk $ \agntRisk1 ->
       forAll genRisk $ \agntRisk2 ->
       payoffLDO > 0 && payoffStETHHolders < 0 ==>
       let params = (modelHeterogenousGameParametersTestProposalAsymmetric payoffLDO payoffStETHHolders agntRisk1 agntRisk2)  in eqEquilibriumModelHeterogenousGame params (modelHeterogenousProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) === True


-----------------------------------------------------------
-- 6. ModelBayesian (2 stakers with asymmetric information)
-----------------------------------------------------------

-- 5.1 Benefitting only LDO holders - symmetric public and private effect on stakers

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game both agents stake
-- NOTE that this game is equivalent to the modelHeterogenous before
prop_ModelBayesianGameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy stakerMoves params) 0 0 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game both agents do not stake; both perceive it as not damaging
-- NOTE that this game is equivalent to the modelHeterogenous before
prop_ModelBayesianGameNegativeForStakersEq2 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy stakerMoves params) 1 1 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- NOTE the threat of execution is important for staking to be relevant at all
-- Actual uncertainty regarding types
prop_ModelBayesianGameNegativeForStakersEq3 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.5 0.5 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- NOTE the threat of execution is important for staking to be relevant at all
-- Actual uncertainty regarding types
-- And unbalanced initial endowment
prop_ModelBayesianGameNegativeForStakersEq4 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.5 0.5 === True
  where params = (modelBayesianGameParametersTestProposal2 payoffLDO payoffStETHHolders)

-- NOTE the threat of execution is important for staking to be relevant at all
-- Actual uncertainty regarding types; uncertainty is unequally distributed
prop_ModelBayesianGameNegativeForStakersEq5 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.1 0.8 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- Actual uncertainty regarding types; uncertainty is unequally distributed; initial endowment unequally distributed
prop_ModelBayesianGameNegativeForStakersEq6 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy2 stakerMoves params) 0.1 0.8 === True
  where params = (modelBayesianGameParametersTestProposal2 payoffLDO payoffStETHHolders)

-- Actual uncertainty regarding types
-- And unbalanced initial endowment
-- Only "rich" stETH holder sends value to the escrow
prop_ModelBayesianGameNegativeForStakersEq7 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianGame params (modelBayesianProposeExecuteOptimalMinStakingStrategy3 stakerMoves params) 0.5 0.5 === False
  where params = (modelBayesianGameParametersTestProposal2 payoffLDO payoffStETHHolders)


-------------------------------------------------------------------------------
-- 6. ModelBayesianEndogenousSignal 2 stakers asymmetric, objective information 
-------------------------------------------------------------------------------

-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game both agents stake
-- In this game, both players receive perfect signals 
prop_ModelBayesianEndogenousSignalGameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianEndogenousSignalGame params (modelBayesianEndogenousSignalProposeExecuteOptimalMinStakingStrategy stakerMovesSmall params) 1 1 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- In this game, both players receive totally noisy signals
prop_ModelBayesianEndogenousSignalGameNegativeForStakersEq2 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelBayesianEndogenousSignalGame params (modelBayesianEndogenousSignalProposeExecuteOptimalMinStakingStrategy stakerMovesSmall params) 1 1 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)


--------------------------------------------------------------------------------------------------------
-- 7. ModelLeaderFollowerEndogenousSignal - 2 stakers asymmetric, objective information, leader-follower
--------------------------------------------------------------------------------------------------------

-- 7.1 Benefitting only LDO holders - symmetric public and private effect on stakers
-- Test whether a contested proposal goes through
-- NOTE the threat of execution is important for staking to be relevant at all
-- In this game, both players receive totally informative signals; and share the burden
prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelLeaderFollowerEndogenousSignalGame params (modelLeaderFollowerEndogenousSignalProposeExecuteOptimalMinStakingStrategy stakerMovesSmall params) 1 1 === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- In this game, both players receive total noise; so ignore the signal
prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq2 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelLeaderFollowerEndogenousSignalGame params (modelLeaderFollowerEndogenousSignalProposeExecuteIgnoreSignalStrategy stakerMovesSmall params) 0 0  === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)

-- In this game, player1 receives perfect signal; second player follows player1
prop_ModelLeaderFollowerEndogenousSignalGameNegativeForStakersEq3 payoffLDO payoffStETHHolders =
  payoffLDO > 0 && payoffStETHHolders < 0 ==>
  eqEquilibriumModelLeaderFollowerEndogenousSignalGame params (modelLeaderFollowerEndogenousSignalProposeExecuteFollowFirstActionStrategy stakerMovesSmall params) 1 0  === True
  where params = (modelBayesianGameParametersTestProposal payoffLDO payoffStETHHolders)
