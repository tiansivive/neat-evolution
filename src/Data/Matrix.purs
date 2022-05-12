module Data.Matrix
  ( Matrix(..)
  , Vector
  , column
  , columns
  , diff
  , dot
  , element
  , empty
  , fromArray
  , fromColumn
  , fromRow
  , identity
  , identity'
  , madd
  , mapply
  , multiply
  , ncols
  , nrows
  , replicate
  , row
  , rows
  , sum
  , toVector
  , transpose
  , vadd
  )
  where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Debug (spy, traceM)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

-- | Matrix Vector implementation
type Vector = Array
  

-- | Difference between 2 vectors.
-- | Vector have to have the same size
diff :: Vector Number -> Vector Number -> Vector Number
diff xs ys 
  | (A.length xs) /= (A.length ys) = []
  | otherwise = A.zipWith (-) xs ys


-- | Add 2 vectors.
-- | Vector have to have the same size
vadd :: ∀ a. Semiring a => Vector a -> Vector a -> Vector a
vadd xs ys 
  | (A.length xs) /= (A.length ys) = []
  | otherwise = A.zipWith (+) xs ys


-- | Dot product between 2 vectors
-- | https://en.wikipedia.org/wiki/Dot_product
dot :: ∀ a. Semiring a => Vector a -> Vector a -> Maybe a
dot xs ys 
  | (A.length xs) /= (A.length ys) = Nothing
  | otherwise = Just $ sum (A.zipWith (*) xs ys)


-- | Sum vector elements
sum :: ∀ a. Semiring a => Vector a -> a
sum xs = A.foldl (+) zero xs




data Matrix a = Matrix Int Int (Array a)  -- nrow, ncol, values

instance semigroupMatrix :: Semigroup a => Semigroup (Matrix a) where
  append = append

instance monoidMatrix :: Monoid a => Monoid (Matrix a) where
  mempty = Matrix 1 1 mempty

instance functorMatrix :: Functor Matrix where
  map f (Matrix r c v) = Matrix r c $ map f v 




instance eqMatrix :: Eq a => Eq (Matrix a) where 
  eq (Matrix r1 c1 d1)  (Matrix r2 c2 d2) =  r1 == r2 && c1 == c2 && d1 == d2 

instance showMatrix :: Show a => Show (Matrix a) where 
  show (Matrix r c ds) = "Matrix "
    <> show r 
    <> " " <> show c
    <> " " <> show ds

mapply :: forall a b. Monoid b => Semigroup b => Matrix (a -> b) -> Matrix a -> Matrix b
mapply m1@(Matrix m1r m1c v1) m2@(Matrix m2r m2c v2)
    | m1r == m2r && m1c == m2c = Matrix m1r m1c $ A.zipWith ($) v1 v2
    | m2c == m1r =  Matrix (nrows m2) (ncols m1) $ apply' (columns m1) (rows m2)
        where 
            apply' colsM1 rowsM2 = do 
                c <- colsM1 
                r <- rowsM2
                let zippedApplications = A.zipWith ($) c r
                pure $ A.foldl append mempty zippedApplications
    | otherwise = unsafePartial $ crashWith "Matrix::apply >> failed to apply matrices of different dimensions: " 
        
           


-- | Number of rows in matrix
nrows :: ∀ a. Matrix a -> Int 
nrows (Matrix r _ _) = r

-- | Number of cols in matrix
ncols :: ∀ a. Matrix a -> Int 
ncols (Matrix _ c _) = c

-- | Convert Matrix to Vector
toVector :: ∀ a. Matrix a -> Vector a 
toVector (Matrix _ _ vs) = vs


-- | Create array of given dimmension containing replicated value
replicate :: ∀ a. Int -> Int -> a -> Maybe (Matrix a )
replicate r c v | r > 0 && c > 0 = Just $ Matrix r c (A.replicate (r * c) v)
                | otherwise = Nothing


-- | Create array of given dimmension with all values set to mempty
empty :: forall a. Monoid a => Int -> Int -> Matrix a
empty r c = Matrix r' c' (A.replicate (r' * c') mempty)
  where
    r' = if r > 0 then r else 1
    c' = if c > 0 then c else 1


-- | Create identy matrix
identity :: Int -> Matrix Number 
identity n = Matrix n n (identity' n)

identity' :: Int -> Vector Number 
identity' n = do 
  r <- A.range 1 n
  c <- A.range 1 n
  pure $ if r == c then 1.0 else 0.0


-- | Create Matrix from Array
fromArray :: ∀ a. Int -> Int -> Array a -> Maybe (Matrix a)
fromArray r c vs | r > 0 && c > 0 && r*c == A.length vs = Just (Matrix r c vs)
                 | otherwise = Nothing


-- | Create matrix from column
fromColumn :: ∀ a. Vector a -> Matrix a 
fromColumn vs = Matrix (A.length vs) 1 vs


-- | Create matrix from row
fromRow :: ∀ a. Vector a -> Matrix a 
fromRow vs = Matrix 1 (A.length vs) vs


-- | Get specific column as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
column :: ∀ a. Int -> Matrix a -> Vector a
column c (Matrix nr nc vs) = A.mapMaybe (\i -> A.index vs (i*nc+c)) (A.range 0 (nr-1))


-- | Get specific row as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
row :: ∀ a. Int -> Matrix a -> Vector a
row r (Matrix nr nc vs) = A.slice i j vs
  where
    i = if r >=0 && r < nr then r*nc else 0
    j = if r >=0 && r < nr then i+nc else 0


-- | Get specific element. Index is 0 based
element :: ∀ a. Int -> Int -> Matrix a -> Maybe a
element r c (Matrix _ nc vs) = A.index vs ((r*nc) + c)


-- | Return list of rows
rows :: ∀ a. Matrix a -> Array (Vector a)
rows mat = do 
  i <- A.range 0 (nrows mat - 1)
  pure $ row i mat


-- | List of columns
columns :: ∀ a. Matrix a -> Array (Vector a)
columns mat = do 
  i <- A.range 0 (ncols mat - 1)
  pure $ column i mat


-- | Multiply 2 matrices. The have to have compatible dimmensions
multiply :: forall a. Monoid a => Semigroup a => Semiring a => Matrix a -> Matrix a -> Matrix a
multiply m1 m2 = mapply ((*) <$> m2) m1


-- | Transpose matrix
transpose :: ∀ a. Matrix a -> Matrix a 
transpose (Matrix r c ds) = Matrix c r ds'
  where 
    ds' = A.concat $ columns (Matrix r c ds) 


-- | Add 2 matrices. The should have the same size


madd :: forall a. Semiring a => Monoid a => Matrix a -> Matrix a -> Matrix a
madd (Matrix r1 c1 vs1) (Matrix r2 c2 vs2)
  | r1 /= r2 || c1 /= c2 =  unsafePartial $ crashWith ("Matrix::add >> tried to add matrices of different dimensions: "<> "\nM1 [r,c]: " <> show [r1, c1] <> "\nM2 [r,c]: " <> show [r2, c2])
  | otherwise = Matrix r1 c1 $ vadd vs1 vs2