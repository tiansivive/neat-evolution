module Simulation
  ( coordinates
  , handleMouseEvents
  , init
  , spawn
  , time
  )
  where

import Prelude

import Board as B
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Creature (create, draw, intersects, update) as C
import Data.Array (replicate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, sequence_)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Geometry (Position(..))
import Simulation.Types (App, Creature)
import Web.Event.Event (EventType(..))
import Web.Event.Event (target, type_) as Evt
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement) as Canvas
import Web.HTML.HTMLElement (offsetLeft, offsetTop) as HTML
import Web.HTML.Window (RequestAnimationFrameId, requestAnimationFrame)
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
        pure $ spy "Coordinates:" $ Pos { x: x - left, y: y - top}

handleMouseEvents :: App EventListener
handleMouseEvents = 
  let
    toggleDebugOptions :: Event -> Position -> Creature -> Creature
    toggleDebugOptions e p c = 
      let 
        hit = C.intersects p 
      in case Evt.type_ e of
        EventType "click" -> if hit c then c { debug = true } else c { debug = false}
        EventType "mousemove" -> if hit c then c { hover = true } else c { hover = false}
        _ -> c
  in do
    { state } <- ask
    lift $ eventListener $ \e -> do
      { creatures } <- Ref.read state
      mp <- sequence $ coordinates e
      case mp of
        Nothing -> error $ "Could not calculate coordinates for mouse event"
        Just p -> Ref.modify_ _ { creatures = toggleDebugOptions e p <$> creatures } state
    


time :: App RequestAnimationFrameId
time = do
  traceM "Time update"
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  
  B.draw
  sequence_ $ map C.draw updated

  let next = void $ runReaderT time deps
  let eff = setTimeout 250 next
  lift $ requestAnimationFrame (void eff) window


        
step:: App RequestAnimationFrameId
step = do
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  B.draw 
  sequence_ $ map C.draw updated
  lift $ requestAnimationFrame (runReaderT (void step) deps) window


spawn :: Int -> App (Array Creature)
spawn n = sequence $ replicate n C.create 


init :: Int -> App RequestAnimationFrameId
init n = do 
  new <- spawn n
  { state } <- ask
  lift $ Ref.modify_ _ { creatures = new } state
  step
 