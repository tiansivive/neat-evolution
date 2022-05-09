module Main
  ( 
   main
  )
  where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Simulation (handleMouseEvents, loop)
import Simulation.Closeup (draw, step) as CloseUp
import Simulation.Types (UiState(..))
import Simulation.UI (handleReset)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLDocument (toDocument, toParentNode)
import Web.HTML.HTMLInputElement (fromElement, value) as Input
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.MouseEvent.EventTypes (mousemove)



main :: Effect Unit
main = unsafePartial $ do
    log "Booting up!"
    w <- window
    doc <- document w
    mainCanvas <- fromJust <$> getCanvasElementById "board"
    closeUpCanvas <- fromJust <$> getCanvasElementById "closeup"
    mainCanvasEl <- fromJust <$> getElementById "board" (toNonElementParentNode $ toDocument doc)
    ctx <- getContext2D mainCanvas

  
    wInput <- fromJust <$> querySelector (QuerySelector "input[name=width]") (toParentNode doc)
    hInput <- fromJust <$> querySelector (QuerySelector "input[name=height]") (toParentNode doc)
    cInput <- fromJust <$> querySelector (QuerySelector "input[name=creatures]") (toParentNode doc)
    resetBtn <- fromJust <$> querySelector (QuerySelector "#config button") (toParentNode doc)

    width <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement wInput)
    height <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement hInput)
    n <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement cInput)

    dummy <- requestAnimationFrame (pure unit) w
    state <- Ref.new 
      { creatures: []
      , selected: []
      , closeUp: Nothing
      , habitat: { width, height }
      , ui: Init { population: n, habitat: { width, height } }
      }

    let env = { state, ctx, window: w, frameId: dummy}
    let target = toEventTarget mainCanvasEl 
    
    handler <- runReaderT handleMouseEvents env
    addEventListener click handler true target
    addEventListener mousemove handler true target
    
    let eff = getContext2D closeUpCanvas >>= \closeUpCtx -> runReaderT CloseUp.step env { ctx = closeUpCtx } >>= \_ ->  runReaderT loop env 
    
    void $ requestAnimationFrame (void eff) w
    
    let btnTarget = toEventTarget resetBtn
    handlerBtn <- runReaderT handleReset env
    addEventListener click handlerBtn true btnTarget



