module Board where

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
    { board } <- lift $ Ref.read state
    lift do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: toNumber board.width
            , height: toNumber board.height
            }

checkOutOfBounds :: BoundingBox -> App Boolean
checkOutOfBounds { max, min } = do
    { state } <- ask
    { board } <- lift $ Ref.read state
    pure $ ( max.x >= toNumber board.width) 
        ||  (min.x <= 0.0) 
        ||  (max.y >= toNumber board.height) 
        ||  (min.y <= 0.0) 


