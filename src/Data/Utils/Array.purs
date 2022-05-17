module Data.Utils.Array where

import Prelude

import Data.Array (delete, length, unsafeIndex, cons)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Partial.Unsafe (unsafePartial)


randomIndex :: forall a. Array a -> Effect a
randomIndex xs = unsafePartial $ do
    i <- randomInt 0 (length xs -1)
    pure $ unsafeIndex xs i

randomlyTake :: forall a. Eq a => Int -> Array a -> Effect (Array a) 
randomlyTake 0 _ = pure []
randomlyTake x xs = do
    a <- randomIndex xs
    rest <- randomlyTake (x - 1) (delete a xs)
    pure $ cons a rest

randomPairs :: forall a. Array a -> Effect (Array (Tuple a a))
randomPairs gs = for gs \g -> unsafePartial $ do
    g2 <- randomIndex gs
    pure $ Tuple g g2
  