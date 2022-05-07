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
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Geometry (Position(..))
import Simulation.Types (App, Creature)
import Web.Event.Event (EventType(..)) 
import Web.Event.Event (target, type_)  as Evt
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement) as Canvas
import Web.HTML.HTMLElement (offsetLeft, offsetTop) as HTML
import Web.HTML.Window (requestAnimationFrame)
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
        let p = Pos { x: x - left, y: y - top}
        log $ "Coordinates: " <> show p
        pure p

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
        Nothing -> log $ "Could not calculate coordinates for mouse event"
        Just p -> Ref.modify_ _ { creatures = toggleDebugOptions e p <$> creatures } state
    


time :: App Unit
time = do
  lift $ log "running update"

  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  
  B.draw
  sequence_ $ map C.draw updated

  let next = (runReaderT time) deps
  let eff = setTimeout 100 next
  void $ lift $ requestAnimationFrame (void eff) window


        
step:: App Unit
step = do
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  B.draw 
  sequence_ $ map C.draw updated
  void $ lift $ requestAnimationFrame (runReaderT step deps) window


spawn :: Int -> App (Array Creature)
spawn n = sequence $ replicate n C.create 


init :: Int -> App Unit
init n = do 
  new <- spawn n
  { state } <- ask
  lift $ Ref.modify_ _ { creatures = new } state
  step
 