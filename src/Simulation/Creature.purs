module Creature
  ( boundingBox
  , collided
  , create
  , dispatch
  , draw
  , intersects
  , move
  , randomRotate
  , rotate
  , update
  )
  where

import Prelude

import Color (rgb, toHexString)
import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (delete, find)
import Data.Int (toNumber)
import Data.Maths (Degrees)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Vector (Two, Vec(..), scale, vAdd, x, y, distance)
import Data.Vector as V
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref as Ref
import Geometry (BoundingBox, Position(..))
import Graphics.Canvas (arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Habitat (checkOutOfBounds, hitEdge)
import Math as Number
import Simulation.Types (Action(..), App, Creature)




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
        let vision = { left: V.rotate (-30.0) orientation, right: V.rotate 30.0 orientation }
        pure $ { color, radius, orientation, speed, pos, vision, debug: false, hover: false }

       


boundingBox :: Creature -> BoundingBox
boundingBox c = {
    min: { x: x c.pos - c.radius, y: y c.pos - c.radius },
    max: { x: x c.pos + c.radius, y: y c.pos + c.radius }
}


drawLine :: String -> Vec Two Number -> Vec Two Number -> App Unit
drawLine color p1 p2 = do
    { ctx } <- ask
    lift $ do
        beginPath ctx
        setStrokeStyle ctx color
        moveTo ctx (x p1) (y p1)
        lineTo ctx (x p2) (y p2)
        closePath ctx
        stroke ctx

draw :: Creature -> App Unit
draw { pos, radius, color, orientation, vision, debug, hover } = do
    { ctx } <- ask
    if debug || hover then do 
        let tip = (radius * 20.0) `scale` orientation `vAdd` pos
        let left = (radius * 10.0) `scale` vision.left `vAdd` pos
        let right = (radius * 10.0) `scale` vision.right `vAdd` pos

        drawLine "red" pos tip 
        drawLine "blue" pos left
        drawLine "blue" pos right
        lift $ do
            setFillStyle ctx $ "black"
            fillPath ctx $ arc ctx 
                { x: x pos
                , y: y pos
                , radius: radius + 5.0
                , start: 0.0
                , end: 2.0 * Number.pi
                }
    else pure unit
    lift $ do
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
rotate angle c = c 
    { orientation = r c.orientation 
    , vision = { left: r c.vision.left, right: r c.vision.right }
    } 
    where r = V.rotate angle


randomRotate ::  Creature -> App Creature
randomRotate c = tailRecM go c
    where
        go _c = do
            angle <- lift $ randomRange 0.0 360.0
            
            let rotated = rotate angle _c
            out <- checkOutOfBounds $ boundingBox $ move rotated
      
            pure $ if out 
                then Loop _c
                else Done $ move rotated


dispatch :: Action -> Creature -> App Creature
dispatch Move c = pure $ move c
dispatch (HitEdge _) c = randomRotate c
dispatch (Collided _) c = randomRotate c 

   
nextAction :: Creature -> App Action
nextAction c = do 
    { state } <- ask
    { creatures } <- lift $ Ref.read state
    let bb = boundingBox c
    me <- hitEdge bb
    let mc = Collided <$> find (collided c) (delete c creatures)
    pure $ fromMaybe Move (HitEdge <$> me <|> mc)


      




update :: Creature -> App Creature
update c = do
    a <- nextAction c
    dispatch a c


intersects :: Position -> Creature -> Boolean
intersects (Pos { x, y }) c =  
    x >= bounds.min.x - fatFingerThreshold
    && x <= bounds.max.x + fatFingerThreshold
    && y >= bounds.min.y - fatFingerThreshold
    && y <= bounds.max.y + fatFingerThreshold
    where 
        bounds = boundingBox c
        fatFingerThreshold = 5.0

collided :: Creature -> Creature -> Boolean
collided c1 c2 = (distance c1.pos c2.pos) <= c1.radius + c2.radius 

