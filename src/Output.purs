-- | Format and display the simulation results
module Output where

import Prelude

import Data.Array (index, length, mapWithIndex, range, zip, zipWith, filter, drop, null)
import Data.Foldable (for_, sum)
import Data.Int (toNumber, round)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Model (ExperimentState)


-- | Display the experiment results
displayResults :: ExperimentState -> Effect Unit
displayResults state = do
  log "\n=== Franck-Hertz Experiment Results ==="
  log $ "Total electrons collected: " <> show state.collectedCount
  log $ "Total time steps: " <> show state.timeStep
  
  -- Display the I-V curve (current vs voltage)
  let 
    results = zip state.voltageSteps state.readings
    
    formatDataRow :: Tuple Number Number -> String
    formatDataRow (Tuple voltage current) =
      show voltage <> "V: " <> show current <> " mA " <> makeBar current
    
    makeBar :: Number -> String
    makeBar value =
      let barLength = round (value * 50.0)
      in joinWith "" (map (const "*") (range 0 barLength))
    
    -- Create ASCII art for I-V curve
    ivCurveData = map formatDataRow results
  
  log "\n=== Current-Voltage Curve ==="
  log "Voltage (V) : Current (mA)"
  log (joinWith "\n" ivCurveData)
  
  -- Calculate voltage differences between current peaks/dips
  let 
    findFeatures :: Array Number -> Array (Tuple Number String)
    findFeatures readings =
      let 
        indexed = zip state.voltageSteps readings
        
        -- Simple peak/dip detection
        isPeak :: Int -> Boolean
        isPeak i =
          let 
            prev = fromMaybe 0.0 (index readings (i-1))
            curr = fromMaybe 0.0 (index readings i)
            next = fromMaybe 0.0 (index readings (i+1))
          in curr > prev && curr > next
          
        isDip :: Int -> Boolean
        isDip i =
          let 
            prev = fromMaybe 0.0 (index readings (i-1))
            curr = fromMaybe 0.0 (index readings i)
            next = fromMaybe 0.0 (index readings (i+1))
          in curr < prev && curr < next
          
        features = mapWithIndex (\i (Tuple v _) -> 
          if isPeak i then Tuple v "peak"
          else if isDip i then Tuple v "dip"
          else Tuple v "") indexed
      in
        filter (\(Tuple _ label) -> label /= "") features
  
  let features = findFeatures state.readings
  
  log "\n=== Identified Features ==="
  for_ features \(Tuple voltage featureType) ->
    log $ featureType <> " at " <> show voltage <> "V"
  
  -- Calculate voltage spacing between features
  let 
    peakVoltages = map (\(Tuple v _) -> v) $ 
      filter (\(Tuple _ t) -> t == "peak") features
      
    voltageDifferences = zipWith (-) (drop 1 peakVoltages) peakVoltages
  
  unless (null voltageDifferences) do
    log "\n=== Analysis ==="
    log $ "Average voltage spacing between peaks: " <> 
         show (sum voltageDifferences / toNumber (length voltageDifferences)) <> "V"
    log $ "This corresponds to the mercury excitation potential (~4.9 eV expected)"
