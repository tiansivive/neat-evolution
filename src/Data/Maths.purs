module Data.Maths where

import Prelude
import Math (Radians, pi)

type Degrees = Number

toRads :: Degrees -> Radians
toRads = (*) (pi/180.0)