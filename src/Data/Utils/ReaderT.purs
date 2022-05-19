module Data.Utils.ReaderT where

import Prelude

import Control.Monad.Reader (Reader, ReaderT, runReader)
import Control.Monad.Reader.Trans (asks)

fromReader :: forall m r a. Monad m => Reader r a -> ReaderT r m a
fromReader = asks <<< runReader