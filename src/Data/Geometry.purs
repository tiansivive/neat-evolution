module Geometry where

import Prelude

import Math (Radians, pi)



newtype Position = Pos { x :: Number, y :: Number } 
derive newtype instance showPos :: Show Position

type Degrees = Number
type BoundingBox = 
  { min :: { x :: Number, y :: Number }
  , max :: { x :: Number, y :: Number }
  }

toRads :: Degrees -> Radians
toRads = (*) (pi/180.0)