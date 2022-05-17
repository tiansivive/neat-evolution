module Data.Utils.Array where

import Prelude

import Data.Array (length, unsafeIndex, cons)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Partial.Unsafe (unsafePartial)


randomIndex :: forall a. Array a -> Effect Int
randomIndex xs = randomInt 0 (length xs -1)


randomElem :: forall a. Array a -> Effect a
randomElem xs = unsafePartial $ do
    i <- randomIndex xs
    pure $ unsafeIndex xs i

randomlyTake :: forall a. Int -> Array a -> Effect (Array a) 
randomlyTake 0 _ = pure []
randomlyTake x xs = do
    a <- randomElem xs
    rest <- randomlyTake (x - 1) xs
    pure $ cons a rest

randomPairs :: forall a. Array a -> Effect (Array (Tuple a a))
randomPairs gs = for gs \g -> unsafePartial $ do
    g2 <- randomElem gs
    pure $ Tuple g g2
  