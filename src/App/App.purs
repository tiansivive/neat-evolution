module App.App
  ( App
  , AppState(..)
  , Bounds
  , Environment
  , State
  )
  where

import Prelude


import Control.Monad.Reader (ReaderT)
import Effect (Effect)
import Effect.Ref (Ref)
import Graphics.Canvas (Context2D)
import Simulation (Simulation, Config)
import Simulation.Closeup (Inspector)
import Web.HTML (Window)
import Web.HTML.Window (RequestAnimationFrameId)

type Bounds = { w:: Number, h:: Number}


data AppState = Configuring (Record (Config ())) | Running | Paused

type Environment = 
    { state         :: Ref State
    , ctx           :: Context2D    
    , window        :: Window
    , frameId       :: RequestAnimationFrameId
    }

-- TODO: Encode the relationship between closeUp and selected into the type signature, perhaps via some ADT
type State =
    { simulation        :: Simulation Unit
    , appState          :: AppState
    , inspector         :: Record (Inspector ())
    }


type App = ReaderT Environment Effect
