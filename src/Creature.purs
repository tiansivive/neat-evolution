module Creature
 
  where

import Prelude

import Board (checkOutOfBounds)
import Color (Color, toHexString)
import Data.Int (toNumber)
import Data.Vector (Two, Vec, scale, vAdd, x, y)
import Data.Vector as V
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Geometry (Degrees, BoundingBox)
import Graphics.Canvas (Context2D, arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Math as Number



type Creature = 
    { color :: Color
    , radius :: Number
    , pos :: Vec Two Number
    , orientation :: Vec Two Number
    , speed :: Int
    , debug :: Boolean
    } 
data Event = HitEdge | Move


boundingBox :: Creature -> BoundingBox
boundingBox c = {
    min: { x: x c.pos - c.radius, y: y c.pos - c.radius },
    max: { x: x c.pos + c.radius, y: y c.pos + c.radius }
}

draw :: Context2D ->  Creature -> Effect Unit
draw ctx { pos, radius, color, orientation, debug } = do
    if debug then do 
        let tip = 50.0 `scale` orientation `vAdd` pos
        beginPath ctx
        setStrokeStyle ctx "red"
        moveTo ctx (x pos) (y pos)
        lineTo ctx (x tip) (y tip)
        closePath ctx
        stroke ctx
        setFillStyle ctx $ "black"
        fillPath ctx $ arc ctx 
            { x: x pos
            , y: y pos
            , radius: radius + 5.0
            , start: 0.0
            , end: 2.0 * Number.pi
            }
    else pure unit
    setFillStyle ctx $ toHexString color
    fillPath ctx $ arc ctx 
        { x: x pos
        , y: y pos
        , radius
        , start: 0.0
        , end: 2.0 * Number.pi
        }


move :: Creature -> Creature
move c = c 
    { pos = vAdd c.pos $ scale factor c.orientation
    }
    where factor = toNumber c.speed


rotate :: Degrees -> Creature -> Creature
rotate angle c = c { orientation = V.rotate angle c.orientation } 


action :: Event -> Creature -> Effect Creature
action Move c =
    -- log "Moving"
    -- log $ "Current pos: " <> show c.pos
    -- let m = move c
    -- log $ "Moved pos: " <> show m.pos
    pure $ move c
action HitEdge c = do 
    angle <- randomRange 0.0 360.0
    let rc = rotate angle c
    let test = rc { pos = vAdd c.pos $ scale 10.0 rc.orientation }
    -- log $ "Angle: " <> show angle
    -- log $ "Current pos: " <> show c.pos
    -- log $ "Moved rotated pos: " <> show test.pos
    -- log $ "Is out of bounds: " <> (show $ checkOutOfBounds $ boundingBox test)

    if checkOutOfBounds $ boundingBox test
        then action HitEdge c
        else pure $ move rc
   

update :: Creature -> Effect Creature
update c = case checkOutOfBounds $ boundingBox c of
    true -> action HitEdge c
    false -> action Move c    


isContained :: Number -> Number -> Creature -> Boolean
isContained x y c =  
    x >= bounds.min.x - fatFingerThreshold
    && x <= bounds.max.x + fatFingerThreshold
    && y >= bounds.min.y - fatFingerThreshold
    && y <= bounds.max.y + fatFingerThreshold
    where 
        bounds = boundingBox c
        fatFingerThreshold = 5.0
        

-- select :: Number -> Number -> Array Creature -> Array Creature
-- select x y = filter $ isContained x y
   
  