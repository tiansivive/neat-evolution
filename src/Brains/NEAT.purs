module Brains.NEAT
  ( Fit(..)
  , Mutation(..)
  , Population
  , Score
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
import Data.Array (filter, length, sortWith, splitAt, take, unsafeIndex, updateAt, zipWith)
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Utils.Array (randomPairs, randomlyTake)
import Debug as Debug
import Effect (Effect)
import Effect.Random (random, randomInt)
import Effect.Ref as Ref
import FFI.NanoID (customAlphabet)
import Partial.Unsafe (unsafePartial)
import Simulation.Types (Creature, App)

type Population a = Array a
data Mutation = Weight | Bias | Activation



newtype Fit = Fit Creature

type Score = Number

type FitnessFn a = a -> Score
type CutoffFn a = a -> Boolean

fromNum :: Number -> Mutation
fromNum n
    | n > 0.85 = Activation
    | n > 0.5 = Bias
    | otherwise = Weight 


modify :: Mutation -> Gene -> Effect Gene
modify Weight (Gene n) = unsafePartial $ do 
    w <- random
    wi <- randomInt 0 $ (length n.weights) -1
    pure $ Gene n { weights = fromJust $ updateAt wi w n.weights }
modify Bias (Gene n) = do 
    b <- random
    pure $ Gene n { bias = b }
modify Activation (Gene n) = do 
    a <- randomInt 0 (length activationFunctions -1)
    pure $ Gene n { activationFn = a }




mutate :: Genome -> App Genome
mutate g = unsafePartial $ do
    { state } <- ask
    { mutationRate, brainSize } <- lift $ Ref.read state
    r <- lift random
    if r < mutationRate 
        then do
            m <- lift $ fromNum <$> random
            li <- lift $ randomInt 0 (brainSize.layers -1)
            ni <- lift $ randomInt 0 (brainSize.neurons -1)
            _id <- lift $ customAlphabet geneAlphabet geneIdLength
            let l = unsafeIndex g li
            let gen = unsafeIndex l ni
            (Gene next) <- lift $ modify m gen
            let newGene = next { id = _id }
            pure $ fromJust $ updateAt li (fromJust $ updateAt ni (Gene newGene) l) g
        else pure g



crossover :: Tuple Genome Genome -> Effect Genome
crossover (Tuple mom dad) = sequence $ zipWith mergeLayers mom dad
    where 
        mergeLayers l1 l2 = sequence $ zipWith randompick l1 l2
        randompick g1 g2 = do
            r <- random
            pure $ if r >= 0.5 then g1 else g2


produceOffspring :: Array Genome -> Effect (Array Genome)
produceOffspring gs = do 
    couples <- randomPairs gs
    firstChildren <- traverse crossover couples
    secondChildren <- traverse crossover couples
    pure $ firstChildren <> secondChildren




evolve :: FitnessFn Creature -> CutoffFn Creature -> App (Array Creature)
evolve fn cutoff = do
    { state } <- ask
    { creatures } <- lift $ Ref.read state
    let percentageIndexFor x l = floor $ x * (toNumber $ length l)
    --let survivalThreshold = percentageIndexFor 0.5 creatures

    let fit = sortWith fn $ filter cutoff creatures
    let elitism = percentageIndexFor 0.1 fit

    let { before: elites, after: breeding } = splitAt elitism fit
    let genomes = (\c -> c.genome) <$> breeding

    let diff = length creatures - length genomes - length elites
    fillers <- lift $ randomlyTake diff genomes

    offspring <- lift $ produceOffspring (genomes <> fillers)
    mutated <- traverse mutate offspring
    newGeneration <- traverse create mutated
    
    Debug.traceM $ "New gen length: " <> (show $ length newGeneration)
    pure $ take (length creatures) $ elites <> newGeneration

