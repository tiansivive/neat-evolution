module Simulation where

import Prelude

import Board as B
import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Creature (create, draw, intersects, update)
import Data.Array (replicate)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Geometry (Position(..))
import Simulation.Types (App, Creature, State)
import Web.Event.Event (EventType(..), target, type_)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement)
import Web.HTML.HTMLElement (offsetLeft, offsetTop)
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)



coordinates :: Event -> Maybe (Effect Position)
coordinates evt = do
    e <- fromEvent evt
    let x = toNumber $ clientX e
    let y = toNumber $ clientY e

    l <- map toHTMLElement $ target evt >>= fromEventTarget  
    let topE = offsetTop l
    let leftE = offsetLeft l

    pure $ do
        left <- leftE
        top <- topE
        let p = Pos { x: x - left, y: y - top}
        log $ "Coordinates: " <> show p
        pure p

handleMouseEvents :: Partial =>  Ref.Ref State -> (Event -> Position -> Creature -> Creature) -> Event -> Effect Unit
handleMouseEvents state action e = do
  p <- fromJust $ coordinates e
  { creatures } <- Ref.read state
  Ref.modify_ _ { creatures = map (action e p) creatures } state



clickListener :: Partial => Ref.Ref State -> Effect EventListener
clickListener s = eventListener $ handleMouseEvents s toggleDebugOptions
mouseOverListener :: Partial => Ref.Ref State -> Effect EventListener
mouseOverListener s = eventListener $ handleMouseEvents s toggleDebugOptions


toggleDebugOptions :: Event -> Position -> Creature -> Creature
toggleDebugOptions e p c = 
    let hit = intersects p in
    case type_ e of
        EventType "click" -> if hit c then c { debug = true } else c { debug = false}
        EventType "mousemove" -> if hit c then c { hover = true } else c { hover = false}
        _ -> c



time :: App Unit
time = do
  liftEffect $ log "running update"

  deps@{ state, window } <- ask
  { creatures } <- liftEffect $ Ref.read state
  updated <- sequence $ map update creatures
  _ <- liftEffect $ Ref.modify _ { creatures = updated } state
  
  B.draw
  sequence_ $ map draw updated

  let next = (runReaderT time) deps
  let eff = setTimeout 100 next
  void $ liftEffect $ requestAnimationFrame (void eff) window


        
step:: App Unit
step = do
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  B.draw 
  sequence_ $ map draw updated
  void $ lift $ requestAnimationFrame (runReaderT step deps) window




    
spawn :: Int -> App (Array Creature)
spawn n = sequence $ replicate n create 


init :: Int -> App Unit
init n = do 
  new <- spawn n
  { state } <- ask
  lift $ Ref.modify_ _ { creatures = new } state
  step
 