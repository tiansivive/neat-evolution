module Board where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Geometry (BoundingBox)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)


width:: Int
width = 720

height:: Int
height = 400


draw :: Context2D -> Effect Unit
draw ctx = do
    setFillStyle ctx "#EDEDED"
    fillPath ctx $ rect ctx
        { x: 0.0
        , y: 0.0
        , width: toNumber width
        , height: toNumber height
        }

checkOutOfBounds :: BoundingBox -> Boolean
checkOutOfBounds { max, min } = ( max.x >= toNumber width) 
    ||  (min.x <= 0.0) 
    ||  (max.y >= toNumber height) 
    ||  (min.y <= 0.0) 


