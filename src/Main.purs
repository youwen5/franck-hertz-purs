module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Model (ExperimentState, Settings)
import Input (getUserSettings)
import Simulation (simulateVoltageSweep)
import Output (displayResults)
import Random (mkRandomState)

-- | Initialize the experiment state using user-provided settings.
initialState :: Settings -> ExperimentState
initialState settings =
  { 
    electrons: [],            -- Start with no electrons in the system
    collectedCount: 0,
    settings: settings,
    mercuryDensity: 1.0,      -- Default mercury vapor density
    timeStep: 0,
    currentReading: 0.0,
    readings: [],
    voltageSteps: []
  }

runSimulation = do
  -- Get user settings
  settings <- getUserSettings
  
  -- Create initial state
  let state0 = initialState settings
  
  -- Create random state with a seed
  let randomState = mkRandomState 12345
  
  -- Run a voltage sweep from 0V to 30V with 500 steps per voltage setting
  log "\nRunning voltage sweep from 0V to 30V..."
  { finalState, finalRandomState } <- 
    simulateVoltageSweep state0 randomState 0.0 30.0 0.5 500

  finalState

-- | Main function to run the Frank-Hertz experiment simulation
main :: Effect Unit
main = do
  log "Franck-Hertz Experiment Simulation"
  log "=================================="
  
  -- Get user settings
  settings <- getUserSettings
  
  -- Create initial state
  let state0 = initialState settings
  
  -- Create random state with a seed
  let randomState = mkRandomState 12345
  
  -- Run a voltage sweep from 0V to 30V with 500 steps per voltage setting
  log "\nRunning voltage sweep from 0V to 30V..."
  { finalState, finalRandomState } <- 
    simulateVoltageSweep state0 randomState 0.0 30.0 0.5 500
    
  -- Display the results
  displayResults finalState
  
  log "\nSimulation complete!"
