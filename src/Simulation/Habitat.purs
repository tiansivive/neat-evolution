module Habitat
  ( Edge
  , Habitat
  , HabitatRows
  , checkOutOfBounds
  , draw
  , edges
  , hitEdge
  )
  where

import Prelude

import App.Graphics (Graphics)
import Control.Monad.Reader (ask, lift)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Vector (Two, Vec(..))
import Geometry (BoundingBox)
import Graphics.Canvas (fillPath, rect, setFillStyle)




type Edge = 
    { vector :: Vec Two Number
    , point :: Vec Two Number 
    }

type HabitatRows = 
    ( width     :: Int
    , height    :: Int
    )
type Habitat =  Record HabitatRows
 
edges :: Habitat -> { top:: Edge, bottom:: Edge, left :: Edge, right :: Edge  }
edges habitat = { top, bottom, left, right } 
   where
        top     = { vector: Vec [1.0, 0.0], point: Vec [0.0, 0.0] }
        bottom  = { vector: Vec [1.0, 0.0], point: Vec [0.0, toNumber habitat.height] }
        left    = { vector: Vec [0.0, 1.0], point: Vec [0.0, 0.0] }
        right   = { vector: Vec [0.0, 1.0], point: Vec [0.0, toNumber habitat.width] }




hitEdge :: Habitat -> BoundingBox -> Maybe Edge
hitEdge habitat { max, min }  = me
    where 
        es = edges habitat 
        me  | max.x >= toNumber habitat.width   = Just es.right
            | min.x <= 0.0                      = Just es.left
            | max.y >= toNumber habitat.height  = Just es.bottom
            | min.y <= 0.0                      = Just es.top
            | otherwise                         = Nothing



checkOutOfBounds :: Habitat -> BoundingBox -> Boolean
checkOutOfBounds { width, height } { max, min }  = 
    ( max.x >= toNumber width) 
        ||  (min.x <= 0.0) 
        ||  (max.y >= toNumber height) 
        ||  (min.y <= 0.0) 


draw :: forall r. Graphics (habitat :: Habitat | r) Unit
draw = do
    { ctx, habitat } <- ask
    lift do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: toNumber habitat.width
            , height: toNumber habitat.height
            }
