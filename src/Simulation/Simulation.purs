module Simulation
  ( coordinates
  , handleCollisions
  , handleMouseEvents
  , loop
  , sort
  , spawn
  )
  where

import Prelude

import Brains.Genome (network)
import Brains.NEAT (evolve)
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Creature (collided)
import Creature (create, draw, intersects, rotate, update) as C
import Data.Array (delete, filter, find, index, replicate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Vector (x)
import Debug (spy)
import Effect (Effect)
import Effect.Console (error)
import Effect.Random (randomRange)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Geometry (Position(..))
import Habitat as Habitat
import Simulation.Types (App, BrainSize, Creature, SimState(..), State, HabitatConfig)
import Web.DOM.Element (setAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.Event (target, type_) as Evt
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement) as Canvas
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (offsetLeft, offsetTop) as HTML
import Web.HTML.Window (RequestAnimationFrameId, document, requestAnimationFrame)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent) as ME



coordinates :: Event -> Maybe (Effect Position)
coordinates evt = do
    e <- ME.fromEvent evt
    let x = toNumber $ ME.clientX e
    let y = toNumber $ ME.clientY e

    l <- map Canvas.toHTMLElement $ Evt.target evt >>= Canvas.fromEventTarget  
    let topE = HTML.offsetTop l
    let leftE = HTML.offsetLeft l

    pure $ do
        left <- leftE
        top <- topE
        pure $ Pos { x: x - left, y: y - top}

handleMouseEvents :: App EventListener
handleMouseEvents = 
  let
    handleEventFor :: Ref.Ref State -> Event -> Position -> Array Creature -> Effect Unit
    handleEventFor state e p creatures = 
      let 
        hit = C.intersects p 
        toggleDebug c = if hit c then c { debug = true } else c { debug = false}
        toggleHover c =  if hit c then c { hover = true } else c { hover = false}
        updated = toggleDebug <$> creatures
        selected = filter (\c -> c.debug) updated
      in case Evt.type_ e of
        EventType "click" -> Ref.modify_ _ { creatures = updated, selected = spy "Selected" selected, closeUp = spy "closeup" $ index selected 0 } state
        EventType "mousemove" ->  Ref.modify_ _ { creatures = toggleHover <$> creatures } state
        _ -> pure unit
  in do
    { state } <- ask
    lift $ eventListener $ \e -> do
      { creatures } <- Ref.read state
      mp <- sequence $ coordinates e
      case mp of
        Nothing -> error $ "Could not calculate coordinates for mouse event"
        Just p -> handleEventFor state e p creatures
          
          
    


-- time :: App RequestAnimationFrameId
-- time = do
--   traceM "Time update"
--   deps@{ state, window } <- ask
--   { creatures } <- lift $ Ref.read state
--   updated <- sequence $ map C.update creatures
--   _ <- lift $ Ref.modify _ { creatures = updated } state
  
--   Habitat.draw
--   sequence_ $ map C.draw updated

--   let next = void $ runReaderT time deps
--   let eff = setTimeout 250 next
--   lift $ requestAnimationFrame (void eff) window


        
step:: Partial => App RequestAnimationFrameId
step = do
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- traverse C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  Habitat.draw 
  traverse_ C.draw updated
  lift $ requestAnimationFrame (runReaderT (void loop) deps) window
  --lift $ requestAnimationFrame (pure unit) window


spawn :: BrainSize -> Int -> App (Array Creature)
spawn { layers, neurons } n = do
  genomes <- lift $ sequence $ replicate n $ network layers neurons
  traverse C.create genomes


loop :: Partial => App RequestAnimationFrameId
loop = 
  let
    updateCanvas w conf = do
      doc <- document w
      canvas <- fromJust <$> querySelector (QuerySelector "#board") (toParentNode doc)
      setAttribute "width" (show conf.habitat.width) canvas
      setAttribute "height" (show conf.habitat.height) canvas
  in do 
    { state, window } <- ask
    { simulation, brainSize, habitat } <- lift $ Ref.read state
    case simulation of
      Paused -> lift $ requestAnimationFrame (pure unit) window
      Init conf -> do 
        new <- spawn brainSize conf.population
        lift $ updateCanvas window conf
        lift $ Ref.modify_ _ { creatures = new, selected = [], closeUp = Nothing, habitat = conf.habitat, simulation = Playing } state

        _ <- lift $ setTimeout 5000 (Ref.modify_ _ { simulation = Completed } state)

        step
      Playing -> step
      Completed -> do 
        nextGen <- evolve (fitness habitat) (cutoff habitat)
        _ <- lift $ Ref.modify_ _ { creatures = nextGen, simulation = Completed } state
        _ <- lift $ setTimeout 5000 (Ref.modify_ _ { simulation = Completed } state)
        _ <- lift $ Ref.modify_ _ { simulation = Playing } state
        step
   
 

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


fitness :: HabitatConfig -> Creature -> Number
fitness habitat c = min p (toNumber habitat.width - p)
  where p = x c.pos
   

cutoff :: HabitatConfig -> Creature -> Boolean
cutoff habitat c 
  | x c.pos > (toNumber habitat.width * 0.8) = true
  | x c.pos < (toNumber habitat.width * 0.2) = true
  | otherwise = false


sort :: HabitatConfig -> Creature -> Creature -> Ordering
sort habitat c1 c2 = order
  where
    edgeProximity c = toNumber habitat.width - x c.pos
    order 
      | edgeProximity c1 < edgeProximity c2 = GT
      | edgeProximity c1 > edgeProximity c2 = LT
      | otherwise = EQ
  
  
