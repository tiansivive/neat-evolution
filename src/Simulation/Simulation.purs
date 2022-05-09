module Simulation
  ( coordinates
  , handleMouseEvents
  , loop
  , spawn
  , time
  )
  where

import Prelude

import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Creature (create, draw, intersects, update) as C
import Data.Array (filter, index, replicate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence, sequence_)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Geometry (Position(..))
import Habitat as Habitat
import Simulation.Types (App, Creature, State, UiState(..), Config)
import Web.DOM.Element (QuerySelector, setAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.Event (target, type_) as Evt
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement) as Canvas
import Web.HTML.HTMLDocument (HTMLDocument, toParentNode)
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
          
          
    


time :: App RequestAnimationFrameId
time = do
  traceM "Time update"
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  
  Habitat.draw
  sequence_ $ map C.draw updated

  let next = void $ runReaderT time deps
  let eff = setTimeout 250 next
  lift $ requestAnimationFrame (void eff) window


        
step:: Partial => App RequestAnimationFrameId
step = do
  deps@{ state, window } <- ask
  { creatures } <- lift $ Ref.read state
  updated <- sequence $ map C.update creatures
  _ <- lift $ Ref.modify _ { creatures = updated } state
  Habitat.draw 
  sequence_ $ map C.draw updated
  lift $ requestAnimationFrame (runReaderT (void loop) deps) window


spawn :: Int -> App (Array Creature)
spawn n = sequence $ replicate n C.create 


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
    { ui } <- lift $ Ref.read state
    case ui of
      Paused -> lift $ requestAnimationFrame (pure unit) window
      Init conf -> do 
        new <- spawn conf.population
        lift $ updateCanvas window conf
        lift $ Ref.modify_ _ { creatures = new, selected = [], closeUp = Nothing, habitat = conf.habitat, ui = Running } state
        step
      Running -> step
      _ -> step
   
 