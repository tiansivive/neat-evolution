module Simulation
  ( Config
  , Generation
  , Log
  , Seconds
  , Selection
  , SimState(..)
  , Simulation
  , Size
  , Step
  , handleCollisions
  , simStep
  , sort
  , spawn
  )
  where

import Prelude

import App.Graphics (DrawDependencies)
import Brains.Genome (GenomeID, network)
import Brains.NEAT (Config, evolve) as NEAT
import Control.Monad.RWS (RWST, get, modify_, tell)
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Control.Monad.State.Trans (modify)
import Creature (Creature, collided)
import Creature (create, nextAction, rotate, update) as C
import Data.Array (delete, find, replicate, zipWith)
import Data.Int (toNumber)
import Data.List.Lazy (List, singleton)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Vector (x)
import Debug as Debug
import Effect (Effect)
import Effect.Random (randomRange)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Habitat (Habitat) as H
import Type.Function (type ($))
import Type.Row (type (+))


type Seconds = Int
type Size = Int


type Config r = 
    ( population    :: Int
    , totalGens     :: Size
    , ttlGen        :: Seconds
    | NEAT.Config r
    )


type Generation r = { creatures:: Array Creature, genomes :: Map GenomeID Int | r }
type Selection = Generation (elites :: Array GenomeID, scores :: Map GenomeID (Array Number))



data SimState
    = Idle 
    | Start
    | Simulating
    | Evolving
    | Completed


type Log = List Selection 
type Step = Ref $ Generation ( step :: Int, currentGen :: Int, state :: SimState )
type Simulation = RWST (Record $ Config + DrawDependencies ()) Log Step Effect




simStep:: Partial =>  Simulation Unit
simStep = do
    deps@{ totalGens, ttlGen, habitat } <- ask
    r <- get
    { creatures, state, currentGen, step } <- lift $ Ref.read r

    let 
      evolve = lift $ runReaderT (NEAT.evolve (fitness habitat) (cutoff habitat) creatures) deps
      next = if step == 60 * ttlGen -- 60 fps, ttl is in secs
        then do
          tell $ singleton { elites: [], creatures, genomes: empty, scores: empty }
          modify \ref -> Ref.newWithSelf _ { state = Evolving, step = 0 } ref
        else do 
          --Debug.traceM $ "Simulating step: " <> show step <> " for gen " <> show currentGen
        
          let actions = creatures <#> C.nextAction habitat creatures
          modify_ _ { step = step + 1, creatures = zipWith C.update actions creatures }
          

      switch = case state of
        Start -> do
          Debug.traceM "Starting Sim:"
          initialPopulation <- spawn
          modify_ _ { currentGen = 0, step = 0, creatures = initialPopulation, state = Simulating }
          simStep
        Evolving -> do
          Debug.traceM $ "Evolving gen: " <> show currentGen
          nextGen <- evolve
          modify_ _ { currentGen = currentGen + 1, creatures = nextGen, state = Simulating }
          simStep

        Simulating -> do
          next
          simStep 
        _ -> simStep


    if currentGen /= totalGens 
      then switch
      else modify_ _ { state = Completed }
      




spawn :: Simulation (Array Creature)
spawn = do
  { habitat, brainSize, population } <- ask
  genomes <- lift $ sequence $ replicate population $ network brainSize.layers brainSize.neurons
  lift $ traverse (C.create habitat) genomes

 

handleCollisions :: Array Creature -> Effect (Array Creature)
handleCollisions creatures = 
  let
    applyCollisionStrategy h = 
      case collided h `find` (delete h creatures) of
        Nothing -> pure h
        Just _ -> do
          angle <- randomRange 0.0 360.0
          pure $ C.rotate angle h
  in traverse applyCollisionStrategy creatures 


fitness :: H.Habitat -> Creature -> Number
fitness habitat c = min p (toNumber habitat.width - p)
  where p = x c.pos
   

cutoff :: H.Habitat -> Creature -> Boolean
cutoff habitat c 
  | x c.pos > (toNumber habitat.width * 0.8) = true
  | x c.pos < (toNumber habitat.width * 0.2) = true
  | otherwise = false


sort :: H.Habitat -> Creature -> Creature -> Ordering
sort habitat c1 c2 = order
  where
    edgeProximity c = toNumber habitat.width - x c.pos
    order 
      | edgeProximity c1 < edgeProximity c2 = GT
      | edgeProximity c1 > edgeProximity c2 = LT
      | otherwise = EQ
  
  
