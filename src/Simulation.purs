-- | Module: Simulation
-- | Manage the overall simulation loop
module Simulation where

import Prelude
import Data.Array (range, uncons)
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Model (ExperimentState)
import Physics (step)
import Random (RandomState)

-- | Simulate a voltage sweep experiment
simulateVoltageSweep
  :: ExperimentState
  -> RandomState
  -> Number
  -> Number
  -> Number
  -> Int
  -> { finalState :: ExperimentState, finalRandomState :: RandomState }
simulateVoltageSweep initialState initialRandomState startVoltage endVoltage stepSize stepsPerVoltage =
  let
    totalSteps = round ((endVoltage - startVoltage) / stepSize)
    voltageSteps = range 0 totalSteps
      # map (\i -> startVoltage + stepSize * toNumber i)

    -- Run simulation for a single voltage setting
    simulateVoltage :: ExperimentState -> RandomState -> Number -> { state :: ExperimentState, randomState :: RandomState }
    simulateVoltage state randomState voltage = do
      let stateWithVoltage = state { settings { acceleratingVoltage = voltage } }
      simulateSteps stateWithVoltage randomState stepsPerVoltage

    -- Run multiple steps of the simulation
    simulateSteps :: ExperimentState -> RandomState -> Int -> { state :: ExperimentState, randomState :: RandomState }
    simulateSteps state randomState 0 = { state, randomState }
    simulateSteps state randomState steps = do
      let { newState, randomState: newRandomState } = step state randomState

      simulateSteps newState newRandomState (steps - 1)

    -- Run the full voltage sweep - fixed using Array.uncons
    runSweep :: ExperimentState -> RandomState -> Array Number -> { state :: ExperimentState, randomState :: RandomState }
    runSweep state randomState voltages = case uncons voltages of
      Nothing -> { state, randomState }
      Just { head: v, tail: rest } ->
        let
          result = simulateVoltage state randomState v
        in
          runSweep result.state result.randomState rest
    result = runSweep initialState initialRandomState voltageSteps
  in
    -- Run the sweep and return the final state
    { finalState: result.state, finalRandomState: result.randomState }
