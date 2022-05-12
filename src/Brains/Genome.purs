module Brains.Genome
  ( Gene(..)
  , Genome
  , gene
  )
  where

import Prelude

import ActivationFunction (activationFunctions)
import Data.Array as A
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import FFI.NanoID (customAlphabet)

data Gene = Gene 
    { weights :: Array Number
    , bias :: Number
    , activationFn :: Int
    , disabled :: Boolean
    , id :: String
    }

instance showGene :: Show Gene where
  show (Gene { id, weights, bias, disabled }) = "Gene " <> id <> " >>" 
    <> "\nBias: " <> show bias
    <> "\nDisabled: " <> show disabled
    <> "\nWeights: " <> show weights


type Genome = 
    { layers :: Array Gene
    , id :: String
    } 


geneAlphabet :: String
geneAlphabet = "0123456789abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ."
geneIdLength :: Int
geneIdLength = 16



gene :: Int -> Effect Gene
gene size = do
    l <- randomInt 1 size
    let generate = randomRange 0.0 1.0
    weights <- sequence $ A.replicate l generate <> A.replicate (size - l) (pure 0.0)
    bias <- generate
    i <- randomInt 0 (A.length activationFunctions - 1)
    disabled <- (>) 0.0 <$> generate
    id <- customAlphabet geneAlphabet geneIdLength
    pure $ Gene { weights, bias, activationFn: i, disabled, id }
