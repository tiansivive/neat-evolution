module Simulation.Types where

import Prelude

import Brains.Genome (Genome, GenomeID)
import Brains.NeuralNetwork (NeuralNetwork)
import Color (Color)
import Control.Monad.Reader (Reader, ReaderT, asks, runReader)
import Data.Maybe (Maybe)
import Data.Vector (Two, Vec)
import Effect (Effect)
import Effect.Ref (Ref)
import Graphics.Canvas (Context2D)
import Web.HTML (Window)
import Web.HTML.Window (RequestAnimationFrameId)


type Bounds = { w:: Number, h:: Number}

type Environment = 
    { state         :: Ref State
    , ctx           :: Context2D    
    , window        :: Window
    , frameId       :: RequestAnimationFrameId
    }

-- TODO: Encode the relationship between closeUp and selected into the type signature, perhaps via some ADT
type State =
    { creatures         :: Array Creature
    , selected          :: Array Creature
    , closeUp           :: Maybe Creature
    , habitat           :: HabitatConfig
    , simulation        :: SimState
    , brainSize         :: BrainSize
    }

type BrainSize = { layers :: Int, neurons :: Int }

type Creature = 
    { color :: Color
    , radius :: Number
    , pos :: Vec Two Number
    , orientation :: Vec Two Number
    , vision ::
        { left :: Vec Two Number
        , right :: Vec Two Number
        }
    , speed :: Int
    , brain :: NeuralNetwork
    , debug :: Boolean
    , hover :: Boolean
    } 


type Edge = 
    { vector :: Vec Two Number
    , point :: Vec Two Number 
    }


type HabitatConfig =  
    { width     :: Int
    , height    :: Int
    }

type Config = 
    { population    :: Int
    , habitat       :: HabitatConfig
    }


data SimState = Init Config | Playing | Paused | Completed

data Action 
    = HitEdge Edge | Collided Creature | Move 


type App = ReaderT Environment Effect


fromReader :: forall m r a. Monad m => Reader r a -> ReaderT r m a
fromReader = asks <<< runReader