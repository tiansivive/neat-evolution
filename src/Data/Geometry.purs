module Geometry where

import Prelude

import Data.Vector (Two, Vec(..))
import Math (pow, sqrt)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

newtype Position = Pos { x :: Number, y :: Number } 
derive newtype instance showPos :: Show Position
type BoundingBox = 
  { min :: { x :: Number, y :: Number }
  , max :: { x :: Number, y :: Number }
  }



fromVec2 :: Vec Two Number -> Position
fromVec2 (Vec [x, y]) = Pos { x, y }
fromVec2 _ = unsafePartial $ crashWith "Geometry>>fromVec2: wrong vector length"


distance :: Position -> Position -> Number
distance (Pos { x: x1, y: y1 }) (Pos {x: x2, y: y2}) = 
  sqrt $ (squared (x1 - x2)) + (squared (y1 - y2))
    where squared a = pow a 2.0 


