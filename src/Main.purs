module Main
  ( 
   main
  )
  where

import Prelude

import App.App (AppState(..), Control(..), Environment, State, App)
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
import Effect.Ref as Ref
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Record (merge)
import Simulation (SimState(..), Config, simStep)
import Simulation.Closeup (step) as CloseUp
import Simulation.UI (Signal(..), handleBtnClick, handleMouseEvents)
import Web.DOM (Element)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLDocument (toDocument, toParentNode)
import Web.HTML.HTMLInputElement (fromElement, value) as Input
import Web.HTML.Window (RequestAnimationFrameId, document, requestAnimationFrame)
import Web.UIEvent.MouseEvent.EventTypes (mousemove)


simConfig ∷ Int -> Int -> Record (Config ()) 
simConfig width height = { population: 100
                    , totalGens: 100
                    , ttlGen: 1
                    , mutationRate: 0.1
                    , brainSize: { layers: 1, neurons: 2 }
                    , habitat: { width, height }
                    }



initialState :: Int -> Int -> State
initialState w h = 
      { simulation: 
        { creatures: []
        , genomes: empty
        , step: 0
        , currentGen: 0
        , state: Idle
        }
      , control: Animation
      , appState: Configuring $ simConfig w h
      , inspector: { creature: Nothing }
      }
     

main :: Effect Unit
main = unsafePartial $ do
    log "Booting up!"

    { mainCanvas, mainCanvasEl, closeUpCanvas, ctx, w, width, height, resetBtn, playBtn, pauseBtn, firstFrame } <- htmlBoilerplate

    state <- Ref.new $ initialState width height
    { simulation } <- Ref.read state
    let conf = simConfig width height
    let env = { state, ctx, window: w, frameId: firstFrame}
    setupEventListeners env resetBtn pauseBtn playBtn mainCanvasEl
    
    let eff = getContext2D closeUpCanvas >>= 
              \closeUpCtx -> runReaderT CloseUp.step { ctx: closeUpCtx, window: w, creature: Nothing } >>= 
              \_ -> runRWST simStep ( merge { ctx, window: w } conf ) simulation
                -- >>=
                --   \(RWSResult st _ writer) -> Debug.traceM st >>= 
                --   \_ -> traverse_ Debug.traceM writer
    
    void $ requestAnimationFrame (void eff) w
    
    


setupEventListeners :: Environment -> Element -> Element -> Element -> Element -> Effect Unit
setupEventListeners env resetBtn pauseBtn playBtn mainCanvasEl = do
    let resetTarget = toEventTarget resetBtn
    resetHandler <- runReaderT (handleBtnClick Reset) env

    let pauseTarget = toEventTarget pauseBtn
    pauseHandler <- runReaderT (handleBtnClick Pause) env

    let playTarget = toEventTarget playBtn
    playHandler <- runReaderT (handleBtnClick Pause) env

    addEventListener click resetHandler true resetTarget
    addEventListener click pauseHandler true pauseTarget
    addEventListener click playHandler true playTarget

    let target = toEventTarget mainCanvasEl 
    
    handler <- runReaderT handleMouseEvents env
    addEventListener click handler true target
    addEventListener mousemove handler true target


htmlBoilerplate :: Partial => Effect
             { closeUpCanvas :: CanvasElement
             , ctx :: Context2D
             , doc :: HTMLDocument
             , firstFrame :: RequestAnimationFrameId
             , height :: Int
             , mainCanvas :: CanvasElement
             , mainCanvasEl :: Element
             , n :: Int
             , pauseBtn :: Element
             , playBtn :: Element
             , resetBtn :: Element
             , w :: Window
             , width :: Int
             }
htmlBoilerplate =  do
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

    firstFrame <- requestAnimationFrame (pure unit) w

    pure { w, doc, mainCanvas, closeUpCanvas, mainCanvasEl, ctx, width, height, n, resetBtn, pauseBtn, playBtn, firstFrame }
