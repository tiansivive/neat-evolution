module Brains.NeuralNetwork
  ( Network(..)
  , Neuron(..)
  , Out(..)
  , SumNum(..)
  , activationVector
  , biasVector
  , calculate
  , makeNN
  , testNN
  , weightsMatrix
  )
  where

import Prelude

import ActivationFunction (ActivationFn(..))
import Data.Array ((..))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Matrix (Matrix(..), madd, mapply, multiply)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)


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


data Network = NN 
    { hidden :: Array (NEA.NonEmptyArray Neuron) 
    , output :: Array Int
    } 
instance showNN :: Show Network where
  show (NN { hidden, output }) = "Network >>\nHidden:\n" <> show hidden <> "\nOutput:\n" <> show output




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

calculate :: Network -> Array Number -> Maybe (Array SumNum)
calculate (NN { hidden }) input = Just v
    -- if all (\layer -> valid )
    --     then Nothing
    --     else Just v
    where 
        inputMatrix = Matrix (A.length input) 1 (Sum <$> input)
        tuples = map (\l -> tuple3 (weightsMatrix l) (toSumNum <$> activationVector l)  (biasVector l)) hidden  
        step i (Tuple w (Tuple a (Tuple b _))) = mapply a $ multiply w i `madd` b
        (Matrix _ _ v) = A.foldl step inputMatrix tuples


makeNN :: Int -> Int -> Network
makeNN size nLayers = NN 
    { output: 0..(size -1)
    , hidden: A.replicate nLayers layer
    } 
        where layer = NonEmptyArray $ A.replicate size $ Neuron { 
            weights: A.replicate size 1.0
            , activationFn: identity
            , bias: 1.0
            }

testNN :: Network
testNN = NN 
    { output: 0..2
    , hidden: A.replicate 5 
        $ NonEmptyArray $ A.replicate 5 
            $ Neuron { weights: A.replicate 5 0.0, bias: 0.0, activationFn: \n -> n } 
            
       
}

