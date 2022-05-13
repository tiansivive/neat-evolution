module Habitat where

import Prelude

import Control.Monad.Reader (ask, lift)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Vector (Vec(..))
import Effect.Ref as Ref
import Geometry (BoundingBox)
import Graphics.Canvas (fillPath, rect, setFillStyle)
import Simulation.Types ( App, Edge)


edges :: App { top:: Edge, bottom:: Edge, left :: Edge, right :: Edge  }
edges = do
    { state } <- ask
    { habitat } <- lift $ Ref.read state
    let top         = { vector: Vec [1.0, 0.0], point: Vec [0.0, 0.0] }
    let bottom      = { vector: Vec [1.0, 0.0], point: Vec [0.0, toNumber habitat.height] }
    let left        = { vector: Vec [0.0, 1.0], point: Vec [0.0, 0.0] }
    let right       = { vector: Vec [0.0, 1.0], point: Vec [0.0, toNumber habitat.width] }

    pure { top, bottom, left, right }



draw :: App Unit
draw = do
    { ctx, state } <- ask
    { habitat } <- lift $ Ref.read state
    lift do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: toNumber habitat.width
            , height: toNumber habitat.height
            }

checkOutOfBounds :: BoundingBox -> App Boolean
checkOutOfBounds { max, min } = do
    { state } <- ask
    { habitat } <- lift $ Ref.read state
    pure $ ( max.x >= toNumber habitat.width) 
        ||  (min.x <= 0.0) 
        ||  (max.y >= toNumber habitat.height) 
        ||  (min.y <= 0.0) 


hitEdge :: BoundingBox -> App (Maybe Edge)
hitEdge { max, min } = do
    { state } <- ask
    { habitat } <- lift $ Ref.read state
    e <- edges

    let a   | max.x >= toNumber habitat.width   = Just e.right
            | min.x <= 0.0                      = Just e.left
            | max.y >= toNumber habitat.height  = Just e.bottom
            | min.y <= 0.0                      = Just e.top
            | otherwise                         = Nothing
    pure a 

