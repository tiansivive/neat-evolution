module Brains.NeuralNetwork
  ( NeuralNetwork(..)
  , Neuron(..)
  , Out(..)
  , SumNum(..)
  , activationVector
  , biasVector
  , fromGenome
  , run
  , weightsMatrix
  )
  where

import Prelude

import ActivationFunction (ActivationFn(..), activationFunctions)
import Brains.Genome (Gene(..), Genome, GenomeID, genome)
import Data.Array as A
import Data.Array.NonEmpty (head, length, toArray) as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(..)) as NEA
import Data.Matrix (Matrix(..), madd, mapply, multiply)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Debug (spy)
import Math (e)
import Partial.Unsafe (unsafePartial)


newtype SumNum = Sum Number


derive newtype instance semiringSumNum :: Semiring SumNum
derive newtype instance showSumNum :: Show SumNum

instance semigroupSumNum :: Semigroup SumNum where
  append = (+)

instance monoidSumNum :: Monoid SumNum where
  mempty = Sum 0.0



data Out = Out Number

data Neuron = Neuron 
    { weights :: Array Number
    , activationFn :: Number -> Number
    , bias :: Number
    } 

instance showNeuron :: Show Neuron where
  show (Neuron { weights,  bias }) = "Neuron >>\n\tWeights: " <> show weights <> "\n\tBias: " <> show bias 


data NeuralNetwork = NN 
    { hidden :: Array (NEA.NonEmptyArray Neuron)
    , id     :: GenomeID 
    } 
instance showNN :: Show NeuralNetwork where
  show (NN { hidden }) = "NeuralNetwork >>\nHidden:\n" <> show hidden 

instance eqNN :: Eq NeuralNetwork where
  eq (NN { id: id1 }) (NN { id: id2 }) = id1 == id2


weightsMatrix :: NEA.NonEmptyArray Neuron -> Matrix SumNum
weightsMatrix layer = Matrix rows cols $ A.concat $ NEA.toArray weightsVectors
    where 
        weightsVectors = map (\(Neuron { weights }) -> Sum <$> weights) layer
        rows = A.length $ NEA.head weightsVectors
        cols = NEA.length weightsVectors

biasVector :: NEA.NonEmptyArray Neuron -> Matrix SumNum
biasVector layer = Matrix (A.length v) 1 v
    where v = NEA.toArray $ map (\(Neuron { bias }) -> Sum bias) layer


toSumNum :: ActivationFn -> SumNum -> SumNum
toSumNum (ActivationFn f) = \(Sum n) -> Sum $ f n
  
activationVector :: NEA.NonEmptyArray Neuron -> Matrix ActivationFn
activationVector layer = Matrix (A.length v) 1 v
    where v = NEA.toArray $ map (\(Neuron { activationFn }) -> ActivationFn activationFn) layer

run :: NeuralNetwork -> Array Number -> Maybe (Array SumNum)
run (NN { hidden }) input = Just v
    -- if all (\layer -> valid )
    --     then Nothing
    --     else Just v
    where 
        inputMatrix = Matrix (A.length input) 1 (Sum <$> input)
        tuples = map (\l -> tuple3 (weightsMatrix l) (toSumNum <$> activationVector l)  (biasVector l)) hidden  
        -- step i (Tuple w (Tuple a (Tuple b _))) = spy "applied fn" $ mapply (spy "activationFns" a) $ spy "added bias" $ (spy "multiplied" $ multiply (spy "weights" w) (spy "input" i)) `madd` (spy "bias" b)
        step i (Tuple w (Tuple a (Tuple b _))) = mapply a $ multiply w i `madd` b
        (Matrix _ _ v) = A.foldl step inputMatrix tuples


fromGenome :: Genome -> NeuralNetwork
fromGenome g = NN { hidden, id: genome g }
  where 
    hidden = unsafePartial $ do
      l <- g
      let n = \(Gene { weights, bias, activationFn }) -> Neuron { weights, bias, activationFn: fromJust $ A.index activationFunctions activationFn }
      [NEA.NonEmptyArray $ n <$> l]

