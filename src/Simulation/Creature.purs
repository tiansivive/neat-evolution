module Creature
 
  where

import Prelude

import Board (checkOutOfBounds)
import Color (rgb, toHexString)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ask)
import Data.Int (toNumber)
import Data.Vector (Two, Vec(..), scale, vAdd, x, y)
import Data.Vector as V
import Effect (Effect)


import Effect.Random (randomInt, randomRange)
import Geometry (BoundingBox, Degrees, Position(..))
import Graphics.Canvas (arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Math as Number
import Simulation.Types (App, Creature, Event(..), fromReader)




create :: App Creature
create = ask >>= \{ board: { width, height } } -> 
    let 
        radiusE = map toNumber $ randomInt 5 10
        speedE = randomInt 1 2
        colorE = do
            r <- randomInt 0 255
            g <- randomInt 0 255
            b <- randomInt 0 255
            pure $ rgb r g b 
        posE = do
            r <- radiusE
            x <- randomRange r $ (toNumber width - r)
            y <- randomRange r $ (toNumber height - r)
            pure $ Vec $ [x, y]

        orientationE :: Effect (Vec Two Number)
        orientationE = do
            x <- randomRange 0.0 1.0
            let y = 1.0 - x 
            pure $ Vec [x, y]
    in lift $ do
        radius <- radiusE
        speed <- speedE
        color <- colorE
        pos <- posE
        orientation <- orientationE
        pure $ { color, radius, orientation, speed, pos, debug: false, hover: false }

       


boundingBox :: Creature -> BoundingBox
boundingBox c = {
    min: { x: x c.pos - c.radius, y: y c.pos - c.radius },
    max: { x: x c.pos + c.radius, y: y c.pos + c.radius }
}

draw :: Creature -> App Unit
draw { pos, radius, color, orientation, debug, hover } = ask >>= \{ ctx } -> lift $ do
    if debug || hover then do 
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




action :: Event -> Creature -> App Creature
action Move c = 
    -- log $ "Current pos: " <> show c.pos
    -- let m = move c
    -- log $ "Moved pos: " <> show m.pos
    pure $ move c
action HitEdge c = do
    -- lift $ log "Hit Edge"
    angle <- lift $ randomRange 0.0 360.0 
    let rc = rotate angle c
    let test = move rc
    out <- fromReader $ checkOutOfBounds $ boundingBox test
    -- lift $ log $ "Out: " <> show out

    -- log $ "Angle: " <> show angle
    -- log $ "Current pos: " <> show c.pos
    -- log $ "Moved rotated pos: " <> show test.pos   
    -- log $ "Is out of bounds: " <> (show out)
    if out 
        then action HitEdge c
        else pure $ move rc

   

     
      
update :: Creature -> App Creature
update c = do
    out <- fromReader $ checkOutOfBounds $ boundingBox c 
    case out of
        true -> action HitEdge c
        false -> action Move c    


intersects :: Position -> Creature -> Boolean
intersects (Pos { x, y }) c =  
    x >= bounds.min.x - fatFingerThreshold
    && x <= bounds.max.x + fatFingerThreshold
    && y >= bounds.min.y - fatFingerThreshold
    && y <= bounds.max.y + fatFingerThreshold
    where 
        bounds = boundingBox c
        fatFingerThreshold = 5.0
        

-- select :: Number -> Number -> Array Creature -> Array Creature
-- select x y = filter $ isContained x y
   
  