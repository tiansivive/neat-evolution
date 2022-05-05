module Main
  ( step
  , main
  )
  where

import Prelude

import Board as B
import Creature (Creature, draw, isContained, update)
import Data.Int (toNumber)
import Data.Maybe (fromJust, maybe)
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Simulation (spawn)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (clientLeft, clientTop)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (target)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLCanvasElement (fromElement, fromEventTarget, toElement, toEventTarget, toHTMLElement)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (offsetLeft, offsetTop)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)
import Web.UIEvent.MouseEvent.EventTypes (mousemove, mouseover)

-- import Web.HTML.Window (requestAnimationFrame)

type State = { creatures :: Array Creature }


nCreatures :: Int
nCreatures = 100

handleClick :: Ref.Ref State -> Event -> Effect Unit
handleClick state e = unsafePartial $ do
  { creatures } <- Ref.read state
  let me = fromJust $ fromEvent e
  let l = toHTMLElement $ fromJust $ fromEventTarget $ fromJust $ target e
  top <- offsetTop l
  left <- offsetLeft l

  log $ "(top: " <> show top <> ", left: " <> show left <> ")"
  let hit = isContained ((toNumber $ clientX me) - left) ((toNumber $ clientY me) - top) 
  let update c = if hit c then c { debug = true } else c { debug = false}
  Ref.modify_ _ { creatures = map update creatures } state

  


main :: Effect Unit
main = do
    log "hello"
    w <- window
    doc <- document w
    creatures <- spawn nCreatures 
    mcanvas <- getCanvasElementById "canvas"
    mcanvasEl <- getElementById "canvas" (toNonElementParentNode $ toDocument doc)

    state <- Ref.new { creatures }

    let cb = maybe (log "No canvas element found!") 
          (\canvas -> do
            ctx <- getContext2D canvas
            step w ctx state
            -- time w ctx state
          ) 
          mcanvas
    id <- requestAnimationFrame cb w
    
    let target = unsafePartial $ toEventTarget $ fromJust $ fromElement $ fromJust mcanvasEl 
    listener <- eventListener $ handleClick state
    addEventListener mousemove listener true target

    pure unit


time :: Window -> Context2D -> Ref.Ref State -> Effect Unit
time w ctx state = do
  _ <- setTimeout 100 do
      log "running update"
      B.draw ctx
      { creatures } <- Ref.read state
      updates <- sequence $ map update creatures
      _ <- Ref.modify _ { creatures = updates } state
      _ <- requestAnimationFrame (time w ctx state) w
      sequence_ $ map (draw ctx) updates
   
  pure unit
  
        
step:: Window -> Context2D -> Ref.Ref State -> Effect Unit
step w ctx state = do
  B.draw ctx
  { creatures } <- Ref.read state
  updated <- sequence $ map update creatures
  _ <- Ref.modify _ { creatures = updated } state
  _ <- sequence $ map (draw ctx) updated
  _ <- requestAnimationFrame (step w ctx state) w
  pure unit

