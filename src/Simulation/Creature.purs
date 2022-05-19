module Creature
  ( Action(..)
  , Creature
  , boundingBox
  , collided
  , colorFromString
  , create
  , dispatch
  , draw
  , intersects
  , move
  , nextAction
  , rotate
  , update
  )
  where

import Prelude

import App.Graphics (Graphics)
import Brains.Genome (Gene(..), Genome)
import Brains.NeuralNetwork (NeuralNetwork, SumNum(..), fromGenome, run)
import Color (Color, ColorSpace(..), black, fromHexString, mix, rgb, toHexString)
import Control.Alt ((<|>))
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ask)
import Data.Array (concat, delete, find, foldl, take, unsafeIndex)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.Int.Bits (shl, shr, (.&.))
import Data.Maths (Degrees, toRads)
import Data.Maybe (fromMaybe, maybe)
import Data.Ord (abs)
import Data.String (Pattern(..), length, split)
import Data.String.Unsafe (char)
import Data.Traversable (traverse)
import Data.Vector (Two, Vec(..), scale, vAdd, x, y, distance)
import Data.Vector as V
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Geometry (BoundingBox, Position(..))
import Graphics.Canvas (arc, beginPath, closePath, fillPath, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke)
import Habitat (Habitat, Edge, hitEdge)
import Math (cos, sin)
import Math as Number
import Partial.Unsafe (unsafePartial)



type Creature = 
    { color :: Color
    , radius :: Number
    , pos :: Vec Two Number
    , orientation :: Vec Two Number
    , vision ::
        { left :: Vec Two Number
        , right :: Vec Two Number
        }
    , speed :: Number
    , brain :: NeuralNetwork
    , genome :: Genome
    , debug :: Boolean
    , hover :: Boolean
    } 

data Action 
    = HitEdge Edge | Collided Creature | Move 



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



create :: Habitat -> Genome -> Effect Creature
create habitat g = do
    let 
        radiusE = map toNumber $ randomInt 5 10
        speedE = randomRange 1.0 4.0
        posE = do
            r <- radiusE
            x <- randomRange r $ (toNumber habitat.width - r)
            y <- randomRange r $ (toNumber habitat.height - r)
            pure $ Vec [x, y]
            -- offsetX <- randomRange (-100.0) 100.0
            -- offsetY <- randomRange (-100.0) 100.0

            -- pure $ Vec $ [toNumber habitat.width /2.0 + offsetX, toNumber habitat.height /2.0 + offsetY]

        orientationE :: Effect (Vec Two Number)
        orientationE = do
            x <- randomRange (-1.0) 1.0
            sign <- randomInt 0 1
            let y = (1.0 - abs x) 
            pure $ Vec [x, if sign == 1 then y else (-y)]
        
        genomeColors = traverse (traverse \(Gene { id }) -> fromHexString ("#" <> id)) g
        blend = concat >>> (foldl (\a c -> mix RGB a c 0.5) black)

 
    radius <- radiusE
    speed <- speedE
    pos <- posE
    orientation <- orientationE

    let color = maybe black blend genomeColors
        brain = fromGenome g
        vision = { left: V.rotate (-30.0) orientation, right: V.rotate 30.0 orientation }

    pure $ { color, radius, orientation, speed, pos, vision, brain, genome: g, debug: false, hover: false }

       


boundingBox :: Creature -> BoundingBox
boundingBox c = {
    min: { x: x c.pos - c.radius, y: y c.pos - c.radius },
    max: { x: x c.pos + c.radius, y: y c.pos + c.radius }
}


drawLine :: forall (r :: Row Type). String -> Vec Two Number -> Vec Two Number -> Graphics r Unit
drawLine color p1 p2 = do
    { ctx } <- ask
    lift $ do
        beginPath ctx
        setStrokeStyle ctx color
        moveTo ctx (x p1) (y p1)
        lineTo ctx (x p2) (y p2)
        closePath ctx
        stroke ctx

draw :: forall (r :: Row Type). Creature -> Graphics r Unit
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
move c = c { pos = vAdd c.pos $ scale c.speed c.orientation }



rotate :: Degrees -> Creature -> Creature
rotate angle c = c 
    { orientation = r c.orientation 
    , vision = { left: r c.vision.left, right: r c.vision.right }
    } 
    where r = V.rotate angle


-- randomRotate ::  Creature -> Effect Creature
-- randomRotate c = tailRecM go c
--     where
--         go _c = do
--             { habitat } <- ask
--             angle <- lift $ randomRange 0.0 360.0
            
--             let rotated = rotate angle _c
--             let out = checkOutOfBounds habitat $ boundingBox $ move rotated
      
--             pure $ if out 
--                 then Loop _c
--                 else Done $ move rotated


dispatch :: Action -> Creature -> Creature
dispatch Move c = move c
dispatch (HitEdge _) c = c
dispatch (Collided _) c = move c 

   

nextAction :: Habitat -> Array Creature -> Creature -> Action
nextAction habitat creatures c = fromMaybe Move (HitEdge <$> me <|> Collided <$> mc)
    where
        bb = boundingBox c
        me = hitEdge habitat bb
        mc = find (collided c) (delete c creatures)



update :: Action -> Creature -> Creature
update a c = dispatch a $ c { 
        orientation = o, 
        vision = { left: V.rotate (-30.0) o, right: V.rotate 30.0 o  },
        speed = min speed 4.0
    }
    where 
        --let input = [x c.pos, y c.pos, x c.orientation, y c.orientation, toNumber c.speed]
        input = [x c.orientation, y c.orientation]
        out = take 2 $ fromMaybe (Sum <$> input) $ run c.brain input
        (Sum degrees) = unsafePartial $ unsafeIndex out 0 
        (Sum speed) = unsafePartial $  unsafeIndex out 1 
        r = toRads $ degrees * 360.0
        o =  Vec [cos r, sin r]
   


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

