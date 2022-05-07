module Main
  ( 
   main
  )
  where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import Data.Maybe (fromJust, maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Simulation (clickListener, mouseOverListener, spawn, step, init)
import Simulation.Types (App)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLCanvasElement (fromElement, toEventTarget)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.MouseEvent.EventTypes (mousemove)

-- import Web.HTML.Window (requestAnimationFrame)




nCreatures :: Int
nCreatures = 100



main :: Effect Unit
main = unsafePartial $ do
    log "Booting up!"
    w <- window
    doc <- document w
    mcanvas <- getCanvasElementById "canvas"
    mcanvasEl <- getElementById "canvas" (toNonElementParentNode $ toDocument doc)
    state <- Ref.new { creatures: [] }

    let cb = maybe (log "No canvas element found!") 
          (\canvas -> do
            ctx <- getContext2D canvas
            runReaderT (init nCreatures) { state, ctx, window: w, board: { width: 720, height: 400 } }
          ) 
          mcanvas
    void $ requestAnimationFrame cb w
    
    let target = unsafePartial $ toEventTarget $ fromJust $ fromElement $ fromJust mcanvasEl 
    cl <- clickListener state
    mml <- mouseOverListener state
    addEventListener click cl true target
    addEventListener mousemove mml true target

    


