module Simulation
  ( Animation(..)
  , Config
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

import App.Graphics (DrawDependencies, Graphics, render)
import Brains.Genome (GenomeID, network)
import Brains.NEAT (Config, BrainSize, evolve) as NEAT
import Control.Monad.RWS (RWST, execRWST, get, modify, modify_, runRWST, tell)
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Creature (Creature, collided, create)
import Creature (create, draw, nextAction, rotate, update) as C
import Data.Array (delete, find, replicate, zipWith)
import Data.Int (toNumber)
import Data.List.Lazy (List, singleton)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse, traverse_)
import Data.Vector (x)
import Debug as Debug
import Effect (Effect)
import Effect.Random (randomRange)
import Habitat (Habitat, draw) as H
import Type.Function (type ($))
import Type.Row (type (+))
import Web.HTML.Window (requestAnimationFrame)


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


data Animation = Running | Paused 

data SimState
    = Idle 
    | Start
    | Animation Animation
    | Simulating
    | Evolving
    | Completed


type Log = List Selection 
type Step = Generation ( step :: Int, currentGen :: Int, state :: SimState )
type Simulation = RWST (Record $ Config + DrawDependencies ()) Log Step Effect



draw :: Graphics (creatures:: Array Creature, habitat:: H.Habitat) Unit
draw = do
  deps@{ creatures } <- ask
  lift $ render H.draw deps
  lift $ render (traverse_ C.draw creatures) deps 
 

  

simStep:: Partial => Simulation Unit
simStep = do
    deps@{ totalGens, ttlGen, habitat, ctx, window } <- ask
    st@{ creatures, state, currentGen, step } <- get

    let 
      evolve = lift $ runReaderT (NEAT.evolve (fitness habitat) (cutoff habitat) creatures) deps
      next = if step == 60 * ttlGen -- 60 fps, ttl is in secs
        then do
          tell $ singleton { elites: [], creatures, genomes: empty, scores: empty }
          modify_ _ { state = Evolving, step = 0 }
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
        Animation Paused -> simStep
        Animation Running -> do
          lift $ render draw { creatures, habitat, ctx, window }
          _ <- lift $ requestAnimationFrame (void $ runRWST next deps st) window
          pure unit

        _ -> simStep


    if currentGen /= totalGens 
      then switch
      else modify_ _ { state = Completed }
      


run :: Partial => Record $  Config + DrawDependencies () -> Step -> Effect Unit
run conf st = do
  _ <- runRWST simStep conf st
  pure unit



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
  
  
