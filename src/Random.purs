-- | Utilities for random number generation using a simple linear congruential generator
module Random where

import Prelude
import Data.Int (toNumber)

-- | Simple linear congruential generator state
type RandomState = { seed :: Int }

-- | Initialize a random state with a seed
mkRandomState :: Int -> RandomState
mkRandomState seed = { seed }

-- | Generate a random number between 0 and 1, and return the new state
randomDouble :: RandomState -> { value :: Number, newState :: RandomState }
randomDouble state =
  let
    a = 1664525
    c = 1013904223
    m = 2147483647
    newSeed = (a * state.seed + c) `mod` m
    value = toNumber newSeed / toNumber m
  in
    { value, newState: { seed: newSeed } }

-- | Generate a random boolean with given probability
randomBool :: Number -> RandomState -> { value :: Boolean, newState :: RandomState }
randomBool probability state =
  let
    { value, newState } = randomDouble state
  in
    { value: value < probability, newState }
