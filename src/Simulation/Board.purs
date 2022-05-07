module Board where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.Int (toNumber)
import Effect.Class (liftEffect)
import Geometry (BoundingBox)
import Graphics.Canvas (fillPath, rect, setFillStyle)
import Simulation.Types (Environment, App)



draw :: App Unit
draw = do
    { ctx, board : { width, height } } <- ask
    liftEffect do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: toNumber width
            , height: toNumber height
            }

checkOutOfBounds :: BoundingBox -> Reader Environment Boolean
checkOutOfBounds { max, min } = do
    { board: { width, height }} <- ask
    pure $ ( max.x >= toNumber width) 
        ||  (min.x <= 0.0) 
        ||  (max.y >= toNumber height) 
        ||  (min.y <= 0.0) 


