module Simulation.UI where

import Prelude

import Control.Monad.Reader (ask, lift, runReaderT)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Debug (spy)
import Effect.Ref as Ref
import Graphics.Canvas (getCanvasElementById)
import Partial.Unsafe (unsafePartial)
import Simulation (spawn)
import Simulation.Types (App)
import Web.DOM.Element (setAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML.HTMLDocument (HTMLDocument, toParentNode)
import Web.HTML.HTMLInputElement (fromElement, value) as Input

handleReset :: HTMLDocument -> App EventListener
handleReset doc = let
    handler s _ = unsafePartial $ do

        canvas <- fromJust <$> querySelector (QuerySelector "#board") (toParentNode doc)
        
        wInput <- fromJust <$> querySelector (QuerySelector "input[name=width]") (toParentNode doc)
        hInput <- fromJust <$> querySelector (QuerySelector "input[name=height]") (toParentNode doc)
        cInput <- fromJust <$> querySelector (QuerySelector "input[name=creatures]") (toParentNode doc)


        width <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement wInput)
        height <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement hInput)
        n <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement cInput)
        creatures <- runReaderT (spawn $ spy "New creatures amount" n) s

        setAttribute "width" (show width) canvas
        setAttribute "height" (show height) canvas
        Ref.modify_ _ { creatures = creatures, board = { width, height } } s.state
      
    in do
        env <- ask
        lift $ eventListener $ handler env 
