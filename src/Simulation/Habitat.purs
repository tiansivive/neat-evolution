module Habitat where

import Prelude

import Control.Monad.Reader (ask, lift)
import Data.Int (toNumber)
import Effect.Ref as Ref
import Geometry (BoundingBox)
import Graphics.Canvas (fillPath, rect, setFillStyle)
import Simulation.Types (App)

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


