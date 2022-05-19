module Brains.Genome
  ( Gene(..)
  , Genome
  , GenomeID
  , gene
  , geneAlphabet
  , geneIdLength
  , genome
  , network
  )
  where

import Prelude

import ActivationFunction (activationFunctions)
import Data.Array as A
import Data.Traversable (intercalate, sequence)
import Data.Utils.Array (randomIndex)
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

instance eqGene :: Eq Gene where
  eq (Gene g1) (Gene g2) = g1.id == g2.id
  
instance showGene :: Show Gene where
  show (Gene { id, weights, bias, disabled }) = "\nGene " <> id <> " >>" 
    <> "\n\tBias: " <> show bias
    <> "\n\tDisabled: " <> show disabled
    <> "\n\tWeights: " <> show weights


type Genome = Array (Array Gene)
type GenomeID = String
--     { layers :: Array Gene
--     , id :: String
--     } 


geneAlphabet :: String
--geneAlphabet = "0123456789abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ."
geneAlphabet = "0123456789abcdef"
geneIdLength :: Int
--geneIdLength = 16
geneIdLength = 6




genome :: Genome -> GenomeID
genome g = intercalate " " ids 
  where 
    ids = do
      l <- g
      (Gene { id }) <- l
      pure id
    


gene :: Int -> Effect Gene
gene size = do
    l <- randomInt 1 size
    let generate = randomRange 0.0 1.0
    weights <- sequence $ A.replicate l generate <> A.replicate (size - l) (pure 0.0)
    bias <- generate
    i <- randomIndex activationFunctions
    disabled <- (>) 0.0 <$> generate
    id <- customAlphabet geneAlphabet geneIdLength
    pure $ Gene { weights, bias, activationFn: i, disabled, id }


layer :: Int -> Effect (Array Gene)
layer size = sequence $ A.replicate size (gene size)

network :: Int -> Int -> Effect Genome
network nLayers = sequence <<< A.replicate nLayers <<< layer 
