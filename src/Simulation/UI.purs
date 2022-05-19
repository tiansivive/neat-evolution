module Simulation.UI where

import Prelude

import App.App (App, AppState(..), State)
import Control.Monad.Reader (lift)
import Control.Monad.Reader.Trans (ask)
import Creature (Creature)
import Creature (intersects) as C
import Data.Array (filter, index)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Debug (spy)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref as Ref
import Geometry (Position(..))
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (setAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.Event (target, type_) as Evt
import Web.Event.EventTarget (EventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLCanvasElement (fromEventTarget, toHTMLElement) as Canvas
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (offsetLeft, offsetTop) as HTML
import Web.HTML.HTMLInputElement (fromElement, value) as Input
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent) as ME


data Signal = Reset | Pause | Play


handleBtnClick :: Signal -> App EventListener
handleBtnClick msg = let
    handler Play s _ _ = pure unit
    handler Pause s _ _ = pure unit
    handler Reset s w _ = unsafePartial $ do

        doc <- document w
        wInput <- fromJust <$> querySelector (QuerySelector "input[name=width]") (toParentNode doc)
        hInput <- fromJust <$> querySelector (QuerySelector "input[name=height]") (toParentNode doc)
        cInput <- fromJust <$> querySelector (QuerySelector "input[name=creatures]") (toParentNode doc)
        canvas <- fromJust <$> querySelector (QuerySelector "#board") (toParentNode doc)

        width <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement wInput)
        height <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement hInput)
        n <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement cInput)
        
        setAttribute "width" (show width) canvas
        setAttribute "height" (show height) canvas
        Ref.modify_ _ 
          { appState = Configuring 
            { population: n
            , totalGens: 100
            , ttlGen: 60
            , mutationRate: 0.1
            , brainSize: { layers: 2, neurons: 5 }
            , habitat: { width, height }
            } 
          } s
      
    in do
        { state, window } <- ask
        lift $ eventListener $ handler msg state window


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
        EventType "click" -> pure unit --Ref.modify_ _ { creatures = updated, selected = spy "Selected" selected, closeUp = spy "closeup" $ index selected 0 } state
        EventType "mousemove" -> pure unit --Ref.modify_ _ { creatures = toggleHover <$> creatures } state
        _ -> pure unit
  in do
    { state } <- ask
    lift $ eventListener $ \e -> pure unit
      -- { creatures } <- Ref.read state
      -- mp <- sequence $ coordinates e
      -- case mp of
      --   Nothing -> error $ "Could not calculate coordinates for mouse event"
      --   Just p -> handleEventFor state e p creatures
          
          