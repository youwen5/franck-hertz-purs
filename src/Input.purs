-- | Handle user input for variac voltages and other adjustable parameters.
module Input where

import Prelude

import Data.Array (index)
import Data.Number (fromString)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Model (Settings)
import Node.Process (argv)

-- | Parse command line arguments to get experiment settings
parseArgs :: Array String -> Settings
parseArgs args =
  let
    getArg :: Int -> String -> Number
    getArg idx defaultVal =
      fromMaybe (fromMaybe 0.0 (fromString defaultVal)) do
        arg <- index args idx
        num <- fromString arg
        pure (num)
        
    filamentV = getArg 2 "5.0"    -- Default filament voltage: 5.0V
    acceleratingV = getArg 3 "0.0" -- Default accelerating voltage: 0.0V
    retardingV = getArg 4 "1.5"    -- Default retarding voltage: 1.5V
  in
    { 
      filamentVoltage: filamentV,
      acceleratingVoltage: acceleratingV, 
      retardingVoltage: retardingV
    }

-- | Get user settings from the command line
getUserSettings :: Effect Settings
getUserSettings = do
  args <- argv
  let settings = parseArgs args
  log $ "Running with settings: filament=" <> show settings.filamentVoltage 
       <> "V, accelerating=" <> show settings.acceleratingVoltage 
       <> "V, retarding=" <> show settings.retardingVoltage <> "V"
  pure settings

