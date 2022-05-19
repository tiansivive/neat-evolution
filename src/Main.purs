module Main
  ( 
   main
  )
  where

import Prelude

import App.App (AppState(..))
import Control.Monad.RWS (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (fold, traverse_)
import Data.Int (fromString)
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromJust)
import Debug as Debug
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Record (merge)
import Simulation (SimState(..), simStep)
import Simulation.Closeup (step) as CloseUp
import Simulation.UI (Signal(..), handleBtnClick)
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
    resetBtn <- fromJust <$> querySelector (QuerySelector "#config button[name=reset]") (toParentNode doc)
    pauseBtn <- fromJust <$> querySelector (QuerySelector "#config button[name=pause]") (toParentNode doc)
    playBtn <- fromJust <$> querySelector (QuerySelector "#config button[name=play]") (toParentNode doc)

    width <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement wInput)
    height <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement hInput)
    n <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement cInput)

    dummy <- requestAnimationFrame (pure unit) w

    let simConfig = { population: 100
                    , totalGens: 100
                    , ttlGen: 1
                    , mutationRate: 0.1
                    , brainSize: { layers: 1, neurons: 2 }
                    , habitat: { width, height }
                    }

    state <- Ref.new 
      { simulation: simStep
      , appState: Configuring simConfig
      , inspector: { creature: Nothing }
      }

    let env = { state, ctx, window: w, frameId: dummy}
    -- let target = toEventTarget mainCanvasEl 
    
    -- handler <- runReaderT handleMouseEvents env
    -- addEventListener click handler true target
    -- addEventListener mousemove handler true target
    
    let eff = getContext2D closeUpCanvas >>= 
              \closeUpCtx -> runReaderT CloseUp.step { ctx: closeUpCtx, window: w, creature: Nothing } >>= 
              \_ -> runRWST simStep ( merge { ctx, window: w } simConfig ) { step: 0, currentGen: 0, state: Start, creatures: [], genomes: empty } 
                -- >>=
                --   \(RWSResult st _ writer) -> Debug.traceM st >>= 
                --   \_ -> traverse_ Debug.traceM writer
    
    void $ requestAnimationFrame (void eff) w
    
    let resetTarget = toEventTarget resetBtn
    resetHandler <- runReaderT (handleBtnClick Reset) env

    let pauseTarget = toEventTarget pauseBtn
    pauseHandler <- runReaderT (handleBtnClick Pause) env

    let playTarget = toEventTarget playBtn
    playHandler <- runReaderT (handleBtnClick Pause) env

    addEventListener click resetHandler true resetTarget
    addEventListener click pauseHandler true pauseTarget
    addEventListener click playHandler true playTarget


