module Simulation.Types where

import Prelude

import Color (Color)
import Control.Monad.Reader (Reader, ReaderT, asks, runReader)
import Data.Vector (Two, Vec)
import Effect (Effect)
import Effect.Ref (Ref)
import Graphics.Canvas (Context2D)
import Web.HTML (Window)


type Bounds = { w:: Number, h:: Number}

type Environment = 
    { state         :: Ref State
    , board         :: 
        { width     :: Int
        , height    :: Int
        } 
    , ctx           :: Context2D    
    , window        :: Window
    }

type State =
    { creatures     :: Array Creature }


type Creature = 
    { color :: Color
    , radius :: Number
    , pos :: Vec Two Number
    , orientation :: Vec Two Number
    , speed :: Int
    , debug :: Boolean
    , hover :: Boolean
    } 
data Event = HitEdge | Move

type App = ReaderT Environment Effect

fromReader :: forall m r a. Monad m => Reader r a -> ReaderT r m a
fromReader = asks <<< runReader