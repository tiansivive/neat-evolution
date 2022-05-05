module Simulation where

import Prelude

import Board (checkOutOfBounds, height, width)
import Color (rgb)
import Creature (Creature, move, rotate)
import Data.Array (replicate)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Vector (Two, Vec(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomInt, randomRange)


type Bounds = { w:: Number, h:: Number}


generateCreature :: Effect Creature   
generateCreature = do
    radius <- radiusE
    color <- colorE
    orientation <- orientationE
    speed <- speedE
    pos <- posE
    pure $ { color, radius, orientation, speed, pos, debug: false }

    where 
        colorE = do
            r <- randomInt 0 255
            g <- randomInt 0 255
            b <- randomInt 0 255
            pure $ rgb r g b 
        radiusE = map toNumber $ randomInt 5 10
        posE = do
            r <- radiusE
            x <- randomRange r $ (toNumber width - r)
            y <- randomRange r $ (toNumber height - r)
            pure $ Vec $ [x, y]
        speedE = randomInt 1 2

        orientationE :: Effect (Vec Two Number)
        orientationE = do
            x <- randomRange 0.0 1.0
            let y = 1.0 - x 
            pure $ Vec [x, y]

    
spawn :: Int -> Effect (Array Creature)
spawn n = sequence $ replicate n generateCreature 


