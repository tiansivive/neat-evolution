module Brains.NEAT
  ( BrainSize
  , Config
  , Fit(..)
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
import Brains.NeuralNetwork (NeuralNetwork(..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Creature (Creature, create)
import Data.Array (filter, groupAllBy, length, sortWith, splitAt, take, unsafeIndex, updateAt, zipWithA)
import Data.Array.NonEmpty as NEA
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Utils.Array (randomPairs, randomlyTake)
import Debug as Debug
import Effect (Effect)
import Effect.Random (random, randomInt)
import FFI.NanoID (customAlphabet)
import Habitat (Habitat)
import Partial.Unsafe (unsafePartial)


type Population a = Array a
data Mutation = Weight | Bias | Activation



newtype Fit = Fit Creature

type Score = Number

type FitnessFn a = a -> Score
type CutoffFn a = a -> Boolean

type BrainSize = { layers :: Int, neurons :: Int }


type Config r = 
    ( mutationRate  :: Number
    , brainSize     :: BrainSize
    , habitat       :: Habitat
    | r
    )

type Evolution r = ReaderT (Record (Config r)) Effect

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




mutate :: forall r. Genome -> Evolution r Genome
mutate g = unsafePartial $ do
    { mutationRate, brainSize } <- ask
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
crossover (Tuple mom dad) = zipWithA mergeLayers mom dad
    where 
        mergeLayers = zipWithA randompick
        randompick g1 g2 = do
            r <- random
            pure $ if r >= 0.5 then g1 else g2


produceOffspring :: Array Genome -> Effect (Array Genome)
produceOffspring gs = do 
    couples <- randomPairs gs
    children <- traverse crossover couples
    pure children




evolve :: forall r. FitnessFn Creature -> CutoffFn Creature ->  Array Creature -> Evolution r (Array Creature)
evolve fn cutoff creatures = do
    { habitat } <- ask
    let percentageIndexFor x l = floor $ x * (toNumber $ length l)
    --let survivalThreshold = percentageIndexFor 0.5 creatures

    let fit = sortWith fn $ filter cutoff creatures
        genomes = _.genome <$> fit

        elitism = percentageIndexFor 0.1 fit
        { before: elites, after: breeding } = splitAt elitism genomes

        diff = length creatures - length fit

    fillers <- lift $ randomlyTake diff breeding
    
    -- Debug.traceM $ "Current gen num: " <> (show $ length creatures)
    -- Debug.traceM $ "Fit num: " <> (show $ length fit)
    -- Debug.traceM $ "Elites num: " <> (show $ length elites)
    -- Debug.traceM $ "Breeding num: " <> (show $ length breeding)
    -- Debug.traceM $ "Diff: " <> (show diff)
    -- Debug.traceM $ "Fillers num: " <> (show $ length fillers)

    offspring <- lift $ produceOffspring (breeding <> fillers)
    mutated <- traverse mutate offspring
    newGeneration <- lift $ traverse (create habitat) (mutated <> elites)

    Debug.traceM "\nEvolving!"
    let groups = groupAllBy (\c1 c2 -> compare c1.brain c2.brain) newGeneration
    _ <- for groups \g -> 
        let (NN { id }) = (NEA.head g).brain in 
            Debug.traceM $ "For genome: " <> id <> ": group size -> " <> (show $ NEA.length g)

    
    pure $ take (length creatures) newGeneration

