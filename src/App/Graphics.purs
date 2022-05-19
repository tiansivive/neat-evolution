module App.Graphics where



import Prelude

import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Trans (ask, lift, runReaderT)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Web.HTML.Window (Window)



type DrawDependencies r = ( ctx :: Context2D, window :: Window | r )


type GraphicsT :: forall k. Row Type -> (k -> Type) -> k -> Type
type GraphicsT r = ReaderT (Record (DrawDependencies r))

type Graphics r = GraphicsT r Effect



render :: forall r m a. ReaderT r m a -> r -> m a
render = runReaderT 
