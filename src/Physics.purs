-- | Module: Physics
-- | Implement pure functions for modeling the physical processes:
-- | emission, acceleration, collisions, collection, and one simulation step.
module Physics where

import Prelude
import Data.Array (filter, length, uncons)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Model (Electron, ExperimentState, maxElectrons, mercuryExcitationEnergy)
import Random (RandomState, randomBool, randomDouble)

import Data.Int (round, toNumber)

-- | Emit electrons based on the filament temperature (indirectly from filamentVoltage).
-- Returns an array of new electrons.
emission :: Int -> Number -> RandomState -> { electrons :: Array Electron, newState :: RandomState }
emission timeStep filamentVoltage randomState =
  let
    -- Calculate number of electrons to emit based on filament voltage
    baseEmissionRate = filamentVoltage * 0.5

    -- Add some randomness to the emission rate
    { value: randomFactor, newState: state1 } = randomDouble randomState
    emissionRate = baseEmissionRate * (0.8 + randomFactor * 0.4)

    -- Determine actual number of emissions this step
    numEmissions = min (round emissionRate) maxElectrons

    -- Create the electrons
    makeElectron _ state =
      let
        { value: initialEnergy, newState } = randomDouble state
      in
        { electron: { energy: initialEnergy * 0.1, emittedAt: timeStep, position: 0.0 }
        , newState
        }

    -- Recursively create electrons with updated random state
    createElectrons :: Int -> RandomState -> { electrons :: Array Electron, newState :: RandomState }
    createElectrons 0 state = { electrons: [], newState: state }
    createElectrons n state =
      let
        { electron, newState: newState1 } = makeElectron n state
        { electrons, newState: newState2 } = createElectrons (n - 1) newState1
      in
        { electrons: [ electron ] <> electrons, newState: newState2 }
  in
    createElectrons numEmissions state1

-- | Accelerate an electron based on the accelerating voltage.
accelerate :: Number -> Electron -> Electron
accelerate acceleratingVoltage electron =
  let
    -- Normalize position to calculate portion of voltage experienced
    -- Electrons gain energy proportional to their position in the tube
    energyGain = acceleratingVoltage * 0.01
    newPosition = min 1.0 (electron.position + 0.02)
  in
    electron
      { energy = electron.energy + energyGain
      , position = newPosition
      }

-- | Determine if an electron collides with a mercury atom.
-- If a collision occurs and the electron has enough energy, reduce its energy.
collide :: Number -> RandomState -> Electron -> { electron :: Electron, newState :: RandomState }
collide mercuryDensity randomState electron =
  let
    -- Collision probability increases with mercury density and electron energy
    collisionProbability = mercuryDensity * 0.01 * (if electron.energy > mercuryExcitationEnergy then 0.8 else 0.2)
    { value: doesCollide, newState } = randomBool collisionProbability randomState
  in
    if doesCollide && electron.energy >= mercuryExcitationEnergy then
      -- Inelastic collision: reduce energy by the excitation energy
      { electron: electron { energy = electron.energy - mercuryExcitationEnergy }, newState }
    else
      { electron, newState }

-- | Decide if an electron is collected by the electrode.
-- Only electrons with energy above the retarding voltage are counted.
collected :: Number -> Electron -> Boolean
collected retardingVoltage electron =
  electron.position >= 0.95 && electron.energy >= retardingVoltage

-- | Process collisions for all electrons - fixed to use proper Array handling and field naming
processCollisions :: Array Electron -> Number -> RandomState -> { electrons :: Array Electron, newState :: RandomState }
processCollisions electrons mercuryDensity randomState =
  let
    -- Recursive helper function that returns a consistent field name
    go :: Array Electron -> Array Electron -> RandomState -> { electrons :: Array Electron, newState :: RandomState }
    go processed remaining state =
      case uncons remaining of
        Nothing -> { electrons: processed, newState: state }
        Just { head: e, tail: rest } ->
          let
            { electron: collidedElectron, newState } = collide mercuryDensity state e
            { electrons: newProcessed, newState: newState2 } = go processed rest newState
          in
            { electrons: [ collidedElectron ] <> newProcessed, newState: newState2 }
  in
    go [] electrons randomState

-- | Simulate one time step of the experiment.
step :: ExperimentState -> RandomState -> { newState :: ExperimentState, randomState :: RandomState }
step state randomState =
  let
    -- Emit new electrons based on filament voltage
    { electrons: newElectrons, newState: randomState1 } =
      emission state.timeStep state.settings.filamentVoltage randomState

    -- Combine existing and new electrons, limited to maximum count
    combinedElectrons =
      state.electrons <> newElectrons
        # Array.take maxElectrons

    -- Accelerate all electrons in the system
    acceleratedElectrons =
      map (accelerate state.settings.acceleratingVoltage) combinedElectrons

    -- Process collisions for each electron
    { electrons: collidedElectrons, newState: randomState2 } =
      processCollisions acceleratedElectrons state.mercuryDensity randomState1

    -- Determine electrons that are collected
    collectedElectrons = filter (collected state.settings.retardingVoltage) collidedElectrons

    -- Update collected count
    newCollectedCount = state.collectedCount + length collectedElectrons

    -- Calculate current reading (proportional to collected electrons)
    currentReading = toNumber (length collectedElectrons) * 0.01

    -- Update voltage and reading arrays if this is a measurement step
    newVoltageSteps =
      if state.timeStep `mod` 10 == 0 then state.voltageSteps <> [ state.settings.acceleratingVoltage ]
      else state.voltageSteps

    newReadings =
      if state.timeStep `mod` 10 == 0 then state.readings <> [ currentReading ]
      else state.readings

    -- Remove collected electrons from the active list
    remainingElectrons = filter (\e -> not (collected state.settings.retardingVoltage e)) collidedElectrons

  in
    { newState: state
        { electrons = remainingElectrons
        , collectedCount = newCollectedCount
        , timeStep = state.timeStep + 1
        , currentReading = currentReading
        , readings = newReadings
        , voltageSteps = newVoltageSteps
        }
    , randomState: randomState2
    }
