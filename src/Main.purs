module Main
  ( 
   main
  )
  where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Simulation (handleMouseEvents, init)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLCanvasElement (fromElement, toEventTarget)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.MouseEvent.EventTypes (mousemove)


nCreatures :: Int
nCreatures = 100


main :: Effect Unit
main = unsafePartial $ do
    log "Booting up!"
    w <- window
    doc <- document w
    canvas <- fromJust <$> getCanvasElementById "canvas"
    canvasEl <- fromJust <$> getElementById "canvas" (toNonElementParentNode $ toDocument doc)
    ctx <- getContext2D canvas
    state <- Ref.new { creatures: [] }

    let env = { state, ctx, window: w, board: { width: 720, height: 400 } }
    let target = toEventTarget $ fromJust $ fromElement $ canvasEl 
    
    handler <- runReaderT handleMouseEvents env
    addEventListener click handler true target
    addEventListener mousemove handler true target

    void $ requestAnimationFrame (runReaderT (init nCreatures) env) w
    


