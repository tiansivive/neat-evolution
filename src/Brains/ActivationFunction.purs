module ActivationFunction where

import Prelude

import Math (e, pi, pow, sqrt)




newtype ActivationFn = ActivationFn (Number -> Number)

instance showAcFn :: Show ActivationFn where
  show (ActivationFn f) = "Activation fn results for inputs [0,1]: " <> (show $ f <$> [0.0, 1.0])

infix 8 pow as ^ 


activationFunctions :: Array (Number -> Number)
activationFunctions = [linear, binaryStep, sigmoid, tanh, reLU, leakyReLU, swish, geLU, seLU]


-- From: shorturl.at/aLP46
linear :: Number -> Number
linear = identity
binaryStep :: Number -> Number
binaryStep x 
    | x < 0.0 = 0.0
    | otherwise = 1.0

sigmoid :: Number -> Number
sigmoid x = 1.0 / (1.0 + e^(-x))

tanh :: Number -> Number
tanh x = (e^x - e^(-x)) 
                    / (e^x + e^(-x))

reLU :: Number -> Number
reLU = max 0.0

leakyReLU :: Number -> Number
leakyReLU x = max (0.1 * x ) x

swish :: Number -> Number
swish x = x * sigmoid x


geLU :: Number -> Number
geLU x = 0.5 * x * (1.0 + tanh (sqrt (2.0/pi) * (x + o)))
    where o = 0.044715 * x^3.0


-- From: shorturl.at/tEG25
a :: Number
a = 1.6733
l :: Number
l = 1.0507

seLU :: Number -> Number
seLU x 
    | x >= 0.0 = l * x
    | otherwise = l * a * (e^x - 1.0)
        
            