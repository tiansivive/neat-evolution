module Simulation.Closeup where


import Prelude

import Control.Monad.Reader (ask, lift, runReaderT)
import Creature as C
import Data.Maybe (maybe)
import Data.Vector (Vec(..))
import Effect.Ref as Ref
import Graphics.Canvas (fillPath, rect, scale, setFillStyle, setTransform)
import Simulation.Types (App)
import Web.HTML.Window (RequestAnimationFrameId, requestAnimationFrame)


dimensions :: { height :: Number , width :: Number }
dimensions = { width: 200.0, height: 200.0 }

draw :: App Unit
draw = do
    { ctx, state } <- ask
    { closeUp } <- lift $ Ref.read state

    lift $ do
        setFillStyle ctx "#EDEDED"
        fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: dimensions.width
            , height: dimensions.height
            }
    let scaled c = c { radius = c.radius * 5.0, pos = Vec [100.0, 100.0]}
    maybe (pure unit) (C.draw <<< scaled) closeUp

 
            
step:: App RequestAnimationFrameId
step = do
  deps@{ window } <- ask
  draw 
  lift $ requestAnimationFrame (runReaderT (void step) deps) window




