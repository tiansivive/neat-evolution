module Creature
  ( boundingBox
  , collided
  , colorFromString
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

import Brains.Genome (Genome, genome)
import Brains.NeuralNetwork (SumNum(..), fromGenome, run)
import Color (Color, black, rgb, toHexString)
import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (delete, find, foldl, take, unsafeIndex)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.Int.Bits (shl, shr, (.&.))
import Data.Maths (Degrees, toRads)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), length, split)
import Data.String.Unsafe (char)
import Data.Vector (Two, Vec(..), scale, vAdd, x, y, distance)
import Data.Vector as V

import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref as Ref
import Geometry (BoundingBox, Position(..))
import Graphics.Canvas (arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Habitat (checkOutOfBounds, hitEdge)
import Math (cos, sin)
import Math as Number
import Partial.Unsafe (unsafePartial)
import Simulation.Types (Action(..), App, Creature)



-- | from https://gist.github.com/0x263b/2bdd90886c2036a1ad5bcf06d6e6fb37
-- |
colorFromString :: String -> Color
colorFromString g 
    | length g == 0 = black
    | otherwise = rgb (generateColorComponent 0) (generateColorComponent 1) (generateColorComponent 2) 
        where 
            arr = split (Pattern "") g
            code = toCharCode <<< char
            -- | shift left 5 = multiply by 32 but restricting to a 32 bit number. .&. (AND) to ensure the 32 bits
            _hash h c = let _h = c + (shl h 5 - h) in _h .&. _h 
            hash = foldl _hash 0 $ code <$> arr
            generateColorComponent i = shr hash (i * 8) .&. 255



create :: Genome -> App Creature
create g = do
    { state } <- ask
    { habitat } <- lift $ Ref.read state
    let 
        radiusE = map toNumber $ randomInt 5 10
        speedE = randomInt 1 2
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
        pos <- posE
        orientation <- orientationE
        let color = colorFromString $ genome g
        let brain = fromGenome g
        let vision = { left: V.rotate (-30.0) orientation, right: V.rotate 30.0 orientation }
        pure $ { color, radius, orientation, speed, pos, vision, brain, genome: g, debug: false, hover: false }

       


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
dispatch (HitEdge _) c = pure c
dispatch (Collided _) c = pure c 

   
nextAction :: Creature -> App Action
nextAction c = do 
    { state } <- ask
    { creatures } <- lift $ Ref.read state
    let bb = boundingBox c
    me <- hitEdge bb
    let mc = Collided <$> find (collided c) (delete c creatures)
    pure $ fromMaybe Move (HitEdge <$> me <|> mc)


      

update :: Creature -> App Creature
update c = unsafePartial $ do
    a <- nextAction c
    let input = [x c.pos, y c.pos, x c.orientation, y c.orientation, toNumber c.speed]
    let out = take 1 $ fromMaybe (Sum <$> input) $ run c.brain input
    let (Sum degrees) = unsafeIndex out 0 
    let r = toRads degrees
    dispatch a $ c { orientation = Vec [cos r, sin r] }


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

