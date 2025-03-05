module Main where

import Prelude

import Data.Array (range, length, (!!), filter, zip, drop)
import Data.Foldable (sum)
import Data.Int (toNumber, round)
import Data.Tuple (Tuple(..))
import Model (ExperimentState, Settings)
import Random (mkRandomState, RandomState)
import Simulation (simulateVoltageSweep)
import Physics (step)
import Data.Maybe (fromMaybe)

-- | Initialize the experiment state using user-provided settings.
initialState :: Settings -> ExperimentState
initialState settings =
  { electrons: []
  , -- Start with no electrons in the system
    collectedCount: 0
  , settings: settings
  , mercuryDensity: 1.0
  , -- Default mercury vapor density
    timeStep: 0
  , currentReading: 0.0
  , readings: []
  , voltageSteps: []
  }

-- | Result type for JavaScript interop
type ExperimentResult =
  { voltages :: Array Number
  , currents :: Array Number
  , peakVoltages :: Array Number
  , dipVoltages :: Array Number
  , averageSpacing :: Number
  , totalElectrons :: Int
  , timeSteps :: Int
  }

-- | Function for JavaScript interop that runs a Franck-Hertz experiment with specified parameters
-- | and returns the results in a structured format.
runFranckHertzExperiment :: Number -> Number -> Number -> Number -> Number -> Number -> Int -> ExperimentResult
runFranckHertzExperiment filamentVoltage initialAcceleratingVoltage finalAcceleratingVoltage voltageStep retardingVoltage mercuryDensity randomSeed = do
  -- Create initial settings
  let
    settings =
      { filamentVoltage: filamentVoltage
      , acceleratingVoltage: initialAcceleratingVoltage
      , retardingVoltage: retardingVoltage
      }

  -- Create initial state with custom mercury density
  let state0 = (initialState settings) { mercuryDensity = mercuryDensity }

  -- Create random state with provided seed
  let randomState = mkRandomState randomSeed

  -- Run the voltage sweep
  let result = simulateVoltageSweep state0 randomState initialAcceleratingVoltage finalAcceleratingVoltage voltageStep 500 -- Steps per voltage setting

  -- Analyze results to find peaks and dips
  let
    -- Find peaks in the current-voltage curve
    findPeaks :: Array Number -> Array Number -> Array Number
    findPeaks voltages currents =
      let
        indicesToCheck = range 1 (length currents - 2)

        isPeak :: Int -> Boolean
        isPeak i =
          let
            prev = fromMaybe 0.0 (currents !! (i - 1))
            curr = fromMaybe 0.0 (currents !! i)
            next = fromMaybe 0.0 (currents !! (i + 1))
          in
            curr > prev && curr > next

        peakIndices = filter isPeak indicesToCheck
      in
        peakIndices # map (\i -> fromMaybe 0.0 (voltages !! i))

    -- Find dips in the current-voltage curve
    findDips :: Array Number -> Array Number -> Array Number
    findDips voltages currents =
      let
        indicesToCheck = range 1 (length currents - 2)

        isDip :: Int -> Boolean
        isDip i =
          let
            prev = fromMaybe 0.0 (currents !! (i - 1))
            curr = fromMaybe 0.0 (currents !! i)
            next = fromMaybe 0.0 (currents !! (i + 1))
          in
            curr < prev && curr < next

        dipIndices = filter isDip indicesToCheck
      in
        dipIndices # map (\i -> fromMaybe 0.0 (voltages !! i))

    -- Calculate average spacing between peaks
    calculateAverageSpacing :: Array Number -> Number
    calculateAverageSpacing voltages =
      case length voltages of
        0 -> 0.0
        1 -> 0.0
        n ->
          let
            pairs = zip voltages (drop 1 voltages)
            differences = pairs # map (\(Tuple v1 v2) -> v2 - v1)
            total = sum differences
          in
            total / toNumber (length differences)

    finalState = result.finalState

    peakVoltages = findPeaks finalState.voltageSteps finalState.readings
    dipVoltages = findDips finalState.voltageSteps finalState.readings
    averageSpacing = calculateAverageSpacing peakVoltages

  -- Return the structured result
  identity
    { voltages: finalState.voltageSteps
    , currents: finalState.readings
    , peakVoltages: peakVoltages
    , dipVoltages: dipVoltages
    , averageSpacing: averageSpacing
    , totalElectrons: finalState.collectedCount
    , timeSteps: finalState.timeStep
    }

main
  :: Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Int
  -> { averageSpacing :: Number
     , currents :: Array Number
     , dipVoltages :: Array Number
     , peakVoltages :: Array Number
     , timeSteps :: Int
     , totalElectrons :: Int
     , voltages :: Array Number
     }
main = runFranckHertzExperiment

-- | Create a simulation state with default settings
createDefaultState :: ExperimentState
createDefaultState = initialState
  { filamentVoltage: 6.0
  , acceleratingVoltage: 0.0
  , retardingVoltage: 1.5
  }

-- | Create a random state with a default seed
createDefaultRandomState :: RandomState
createDefaultRandomState = mkRandomState 12345

-- | Perform a single step of the simulation
singleStep :: ExperimentState -> RandomState -> { newState :: ExperimentState, randomState :: RandomState }
singleStep state randomState = step state randomState

-- | Function for JavaScript interop that runs a single step of the simulation
stepSimulation :: ExperimentState -> Number -> { newState :: ExperimentState, randomSeed :: Number }
stepSimulation state randomSeed =
  let
    randomState = mkRandomState (round randomSeed)
    result = singleStep state randomState
    -- Return the new seed so it can be used in subsequent calls
    newSeed = result.randomState.seed
  in
    { newState: result.newState, randomSeed: toNumber newSeed }

