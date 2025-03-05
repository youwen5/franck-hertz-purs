-- | Define the immutable data types for electrons, experiment state, voltages, and physical constants.
module Model where

-- | Represent a single electron.
type Electron =
  { energy :: Number -- Electron energy (in eV)
  , emittedAt :: Int -- Time step when the electron was emitted
  , position :: Number -- Position along the tube (0.0 to 1.0)
  }

-- | Experiment settings that the user can adjust.
type Settings =
  { filamentVoltage :: Number -- Voltage controlling filament heating (affects emission)
  , acceleratingVoltage :: Number -- Voltage difference between cathode and anode
  , retardingVoltage :: Number -- Voltage at collection electrode (to filter low-energy electrons)
  }

-- | The overall state of the experiment at a given time step.
type ExperimentState =
  { electrons :: Array Electron -- List of electrons currently in the system
  , collectedCount :: Int -- Count of electrons that reached the collection electrode
  , settings :: Settings -- Current voltage settings from user input
  , mercuryDensity :: Number -- Parameter representing mercury vapor density
  , timeStep :: Int -- Simulation time step count
  , currentReading :: Number -- Current reading from the collection electrode
  , readings :: Array Number -- Collected current readings over time
  , voltageSteps :: Array Number -- Accelerating voltage values for each reading
  }

-- | Physical constants
mercuryExcitationEnergy :: Number
mercuryExcitationEnergy = 4.9 -- 4.9 eV for mercury vapor excitation

-- | Constants for the simulation
tubeLength :: Number
tubeLength = 1.0 -- Normalized tube length

maxElectrons :: Int
maxElectrons = 1000 -- Maximum number of electrons to track
