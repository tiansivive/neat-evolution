module Brains.Genome
  ( Gene(..)
  , Genome
  , gene
  , genome
  , network
  )
  where

import Prelude

import ActivationFunction (activationFunctions)
import Data.Array (fold)
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

-- data Gene2 = Gene2
--     {  source :: Int
--     ,  sink :: Int
--     ,  weight :: Number
--     ,  activationFn :: Int
--     ,  bias :: Number
--     }

instance showGene :: Show Gene where
  show (Gene { id, weights, bias, disabled }) = "\nGene " <> id <> " >>" 
    <> "\n\tBias: " <> show bias
    <> "\n\tDisabled: " <> show disabled
    <> "\n\tWeights: " <> show weights


type Genome = Array (Array Gene)
--     { layers :: Array Gene
--     , id :: String
--     } 


geneAlphabet :: String
geneAlphabet = "0123456789abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ."
geneIdLength :: Int
geneIdLength = 16




genome :: Genome -> String
genome g = fold ids
    where ids = do
            l <- g
            (Gene { id }) <- l
            pure $ id
    


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


layer :: Int -> Effect (Array Gene)
layer size = sequence $ A.replicate size (gene size)

network :: Int -> Int -> Effect Genome
network nLayers = sequence <<< A.replicate nLayers <<< layer 
