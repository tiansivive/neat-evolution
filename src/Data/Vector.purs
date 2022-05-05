module Data.Vector where

import Prelude

import Control.Apply (lift2)
import Data.Array (index, length, replicate, zipWith)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (fromJust)
import Effect.Unsafe (unsafePerformEffect)
import Geometry (toRads)
import Math (sqrt, cos, sin)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


data Zero
data One
data Two
data Three
data Four

class Sized :: forall k. k -> Constraint
class Sized a where
  sized :: Proxy a -> Int

instance sz :: Sized Zero where
  sized _ = 0
instance s1 :: Sized One where
  sized _ = 1
instance s2 :: Sized Two where
  sized _ = 2
instance s3 :: Sized Three where
  sized _ = 3
instance s4 :: Sized Four where
  sized _ = 4

newtype Vec :: forall k. k -> Type -> Type
newtype Vec s a = Vec (Array a)

instance eqVec :: (Eq a) => Eq (Vec s a) where
  eq (Vec l) (Vec r) = l `eq` r

instance showVec :: (Show a) => Show (Vec s a) where
  show (Vec l) = "Vec " <> show l

instance functorVec :: Functor (Vec s) where
  map f (Vec l) = Vec $ map f l

instance applyVec :: Apply (Vec s) where
  apply (Vec f) (Vec a) = Vec $ zipWith ($) f a 

instance foldableVector :: Foldable (Vec s) where
  foldr f b (Vec xs) = foldr f b xs
  foldl f b (Vec xs) = foldl f b xs
  foldMap f xs = foldr (\b acc -> f b <> acc) mempty xs

fill :: forall s a. EuclideanRing a => Sized s => a -> Vec s a
fill = Vec <<< replicate l 
    where l = sized (Proxy :: Proxy s)

fromArray :: forall s a. Sized s => Array a -> Vec s a
fromArray l = case sized (Proxy :: Proxy s) of
    i | i == length l -> Vec l
      | otherwise     -> unsafePartial $ crashWith "Vector>>fromArray: wrong Array length!"

toArray :: forall s a. Vec s a -> Array a
toArray (Vec a) = a



vAdd :: forall a s. (EuclideanRing a) => Vec s a -> Vec s a -> Vec s a
vAdd = lift2 (+)

vSub :: forall a s. (EuclideanRing a) => Vec s a -> Vec s a -> Vec s a
vSub = lift2 (-)

vMul :: forall a s. (EuclideanRing a) => Vec s a -> Vec s a -> Vec s a
vMul = lift2 (*)

vNegate :: forall a s. (EuclideanRing a) => Vec s a -> Vec s a
vNegate v = negate <$> v

instance semiringVector :: (Sized s) => Semiring (Vec s Number) where
  add = vAdd
  zero = fill 0.0
  mul = vMul
  one = fill 1.0

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number
direction v1 v2 = normalize (vSub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: forall s. Vec s Number -> Number
vlengthSquared v = foldl (+) 0.0 ((\e -> e * e) <$> v)

-- | The length of the given vector: |a|
vlength :: forall s. Vec s Number -> Number
vlength = sqrt <<< vlengthSquared
  -- :: forall a b. f (a -> b) -> f a -> f b

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: forall s. Vec s Number -> Vec s Number
normalize v =
  let im = 1.0 / vlength v
  in ((*) im) <$> v

-- | The distance between two vectors.
distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number
distanceSquared v1 v2 = foldl (+) 0.0 ((\e -> e * e) <$> (vSub v1 v2))

-- | The distance between two vectors.
distance :: forall s. Vec s Number -> Vec s Number -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: forall a s. (EuclideanRing a) => a -> Vec s a -> Vec s a
scale = map <<< (*)


rotate :: Number -> Vec Two Number -> Vec Two Number
rotate angle v = Vec [x', y']
  where
    rads = toRads angle
    x' = ((x v) * cos rads) - ((y v) * sin rads)
    y' = ((x v) * sin rads) + ((y v) * cos rads)


-- | The dot product of a and b
dot :: forall s . Vec s Number -> Vec s Number -> Number
dot v1 v2 = foldl (+) 0.0 (vMul v1 v2)



x ::forall s a. Sized s => Vec s a -> a
x (Vec l) = unsafePartial$ fromJust $ index l 0
      
y ::forall s a. Sized s => Vec s a -> a
y (Vec l) = unsafePartial $ case sized (Proxy :: Proxy s) of
    i | i >= 2      -> fromJust $ index l 1
      | otherwise   -> crashWith $ "Vector>>y: vector has no y component. Size is " <> show i

z ::forall s a. Sized s => Vec s a -> a
z (Vec l) = unsafePartial $ case sized (Proxy :: Proxy s) of
    i | i >= 3      -> fromJust $ index l 2
      | otherwise   -> crashWith $ "Vector>>z: vector has no z component. Size is " <> show i

w ::forall s a. Sized s => Vec s a -> a
w (Vec l) = unsafePartial $ case sized (Proxy :: Proxy s) of
    i | i >= 4      -> fromJust $ index l 3
      | otherwise   -> crashWith $ "Vector>>w: vector has no w component. Size is " <> show i

    