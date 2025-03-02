-- | Module: Simulation
-- | Manage the overall simulation loop
module Simulation where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (range, uncons)
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Model (ExperimentState)
import Physics (step)
import Random (RandomState)

-- | Simulate a voltage sweep experiment
simulateVoltageSweep :: ExperimentState -> RandomState -> Number -> Number -> Number -> Int 
                     -> Effect { finalState :: ExperimentState, finalRandomState :: RandomState }
simulateVoltageSweep initialState initialRandomState startVoltage endVoltage stepSize stepsPerVoltage = do
  let 
    totalSteps = round ((endVoltage - startVoltage) / stepSize)
    voltageSteps = range 0 totalSteps
      # map (\i -> startVoltage + stepSize * toNumber i)
    
    -- Run simulation for a single voltage setting
    simulateVoltage :: ExperimentState -> RandomState -> Number -> Effect { state :: ExperimentState, randomState :: RandomState }
    simulateVoltage state randomState voltage = do
      log $ "Simulating with voltage: " <> show voltage <> "V"
      let stateWithVoltage = state { settings { acceleratingVoltage = voltage } }
      simulateSteps stateWithVoltage randomState stepsPerVoltage
    
    -- Run multiple steps of the simulation
    simulateSteps :: ExperimentState -> RandomState -> Int -> Effect { state :: ExperimentState, randomState :: RandomState }
    simulateSteps state randomState 0 = pure { state, randomState }
    simulateSteps state randomState steps = do
      let { newState, randomState: newRandomState } = step state randomState
      
      -- Log progress for every 100 steps
      when (steps `mod` 100 == 0) do
        log $ "  Step " <> show (stepsPerVoltage - steps) <> "/" <> show stepsPerVoltage 
             <> " - Collected: " <> show newState.collectedCount
             <> " - Current: " <> show newState.currentReading
      
      simulateSteps newState newRandomState (steps - 1)
    
    -- Run the full voltage sweep - fixed using Array.uncons
    runSweep :: ExperimentState -> RandomState -> Array Number -> Effect { state :: ExperimentState, randomState :: RandomState }
    runSweep state randomState voltages = case uncons voltages of
      Nothing -> pure { state, randomState }
      Just { head: v, tail: rest } -> do
        result <- simulateVoltage state randomState v
        runSweep result.state result.randomState rest
  
  -- Run the sweep and return the final state
  result <- runSweep initialState initialRandomState voltageSteps
  pure { finalState: result.state, finalRandomState: result.randomState }
