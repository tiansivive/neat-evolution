module Simulation.Closeup where


import Prelude

import App.Graphics (Graphics)
import Control.Monad.Reader (ask, lift, runReaderT)
import Creature (Creature)
import Creature as C
import Data.Maybe (Maybe, maybe)
import Data.Vector (Vec(..))
import Graphics.Canvas (fillPath, rect, setFillStyle)
import Web.HTML.Window (RequestAnimationFrameId, Window, requestAnimationFrame)


dimensions :: { height :: Number , width :: Number }
dimensions = { width: 200.0, height: 200.0 }




type Inspector r = (creature :: Maybe Creature | r)



draw :: forall (r :: Row Type). Graphics (Inspector r) Unit
draw = do
    { ctx, creature } <- ask
    
    lift $ do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: dimensions.width
            , height: dimensions.height
            }
    let scaled c = c { radius = c.radius * 5.0, pos = Vec [100.0, 100.0]}
    maybe (pure unit) (C.draw <<< scaled) creature

 
            
step:: forall (r :: Row Type). Graphics (Inspector r) RequestAnimationFrameId
step = do
  deps@{ window } <- ask
  draw
  lift $ requestAnimationFrame (runReaderT (void step) deps) window




