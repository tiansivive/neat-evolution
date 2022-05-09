module Creature
 
  where

import Prelude

import Habitat (checkOutOfBounds)
import Color (rgb, toHexString)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Int (toNumber)
import Data.Vector (Two, Vec(..), scale, vAdd, x, y)
import Data.Vector as V
import Debug (spy)
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref as Ref
import Geometry (BoundingBox, Degrees, Position(..))
import Graphics.Canvas (arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Math as Number
import Simulation.Types (App, Creature, Event(..))




create :: App Creature
create = do
    { state } <- ask
    { habitat } <- lift $ Ref.read state
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
            x <- randomRange r $ (toNumber habitat.width - r)
            y <- randomRange r $ (toNumber habitat.height - r)
            pure $ Vec $ [x, y]

        orientationE :: Effect (Vec Two Number)
        orientationE = do
            x <- randomRange 0.0 1.0
            let y = 1.0 - x 
            pure $ Vec [x, y]

    lift $ do
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
        let tip = (radius * 5.0) `scale` orientation `vAdd` pos
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




dispatch :: Event -> Creature -> App Creature
dispatch Move c = pure $ move c
dispatch HitEdge c = tailRecM go c
    where
        go _c = do
            angle <- lift $ randomRange 0.0 360.0
            
            let rotated = rotate angle _c
            out <- checkOutOfBounds $ boundingBox $ move rotated
      
            pure $ if out 
                then Loop _c
                else Done $ move rotated


   

     
      
update :: Creature -> App Creature
update c = do
    out <- checkOutOfBounds $ boundingBox c 
    case out of
        true -> dispatch HitEdge c
        false -> dispatch Move c    


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
   
  