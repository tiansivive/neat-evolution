module Brains.NEAT
  ( Fit(..)
  , Mutation(..)
  , Population
  , crossover
  , evolve
  , fromNum
  , modify
  , mutate
  )
  where

import Prelude

import ActivationFunction (activationFunctions)
import Brains.Genome (Gene(..), Genome, geneAlphabet, geneIdLength)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Creature (create)
import Data.Array (dropEnd, length, reverse, sortBy, splitAt, unsafeIndex, updateAt, zipWith, zipWithA)
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (random, randomInt)
import Effect.Ref as Ref
import FFI.NanoID (customAlphabet)
import Partial.Unsafe (unsafePartial)
import Simulation.Types (Creature, App)

type Population a = Array a
data Mutation = Weight | Bias | Activation



newtype Fit = Fit Creature


type FitnessFn a = a -> a -> Ordering

fromNum :: Number -> Mutation
fromNum n
    | n > 0.85 = Activation
    | n > 0.5 = Bias
    | otherwise = Weight 


modify :: Mutation -> Gene -> Effect Gene
modify Weight (Gene n) = unsafePartial $ do 
    w <- random
    wi <- randomInt 0 $ length n.weights
    pure $ Gene n { weights = fromJust $ updateAt wi w n.weights }
modify Bias (Gene n) = do 
    b <- random
    pure $ Gene n { bias = b }
modify Activation (Gene n) = do 
    a <- randomInt 0 (length activationFunctions)
    pure $ Gene n { activationFn = a }




mutate :: Genome -> App Genome
mutate g = unsafePartial $ do
    { state } <- ask
    { mutationRate, brainSize } <- lift $ Ref.read state
    r <- lift random
    if r < mutationRate 
        then do
            m <- lift $ fromNum <$> random
            li <- lift $ randomInt 0 brainSize.layers
            ni <- lift $ randomInt 0 brainSize.neurons
            _id <- lift $ customAlphabet geneAlphabet geneIdLength
            let l = unsafeIndex g li
            let gen = unsafeIndex l ni
            (Gene next) <- lift $ modify m gen
            let newGene = next { id = _id }
            pure $ fromJust $ updateAt li (fromJust $ updateAt ni (Gene newGene) l) g
        else pure g



crossover :: Genome -> Genome -> Effect Genome
crossover mom dad = sequence $ zipWith mergeLayers mom dad
    where 
        mergeLayers l1 l2 = sequence $ zipWith randompick l1 l2
        randompick g1 g2 = do
            r <- random
            pure $ if r >= 0.5 then g1 else g2




evolve :: FitnessFn Creature -> App (Array Creature)
evolve fn = do
    { state } <- ask
    { creatures } <- lift $ Ref.read state
    let percentageIndexFor x = floor $ x * (toNumber $ length creatures)
    let elitism = percentageIndexFor 0.1
    let survivalThreshold = percentageIndexFor 0.5

    let fit = dropEnd survivalThreshold $ sortBy fn creatures
    let { before: elites, after: breeding } = splitAt elitism fit
    let genomes = (\c -> c.genome) <$> breeding
    offspring <- lift $ zipWithA crossover genomes (reverse genomes)

    newGeneration <- sequence $ create <$> offspring
    pure $ newGeneration <> elites

