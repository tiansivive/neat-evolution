module Simulation.UI where

import Prelude

import Control.Monad.Reader (ask, lift)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Debug (spy)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Simulation.Types (App, SimState(..))
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (fromElement, value) as Input
import Web.HTML.Window (document)


data Signal = Reset | Pause | Play


handleBtnClick :: Signal -> App EventListener
handleBtnClick msg = let
    handler Play s _ _ = Ref.modify_ _ {  simulation = Playing } s
    handler Pause s _ _ = Ref.modify_ _ {  simulation = Paused } s
    handler Reset s w _ = unsafePartial $ do

        doc <- document w
        wInput <- fromJust <$> querySelector (QuerySelector "input[name=width]") (toParentNode doc)
        hInput <- fromJust <$> querySelector (QuerySelector "input[name=height]") (toParentNode doc)
        cInput <- fromJust <$> querySelector (QuerySelector "input[name=creatures]") (toParentNode doc)

        width <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement wInput)
        height <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement hInput)
        n <- (fromJust <<< fromString) <$> (Input.value $ fromJust $ Input.fromElement cInput)
        
        Ref.modify_ _ {  simulation = Init { population: spy "New creatures:" n, habitat: spy "New habitat: " { width, height } } } s
      
    in do
        { state, window } <- ask
        lift $ eventListener $ handler msg state window
