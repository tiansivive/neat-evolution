module Data.Actor where

import Prelude




data Actor :: forall k. (Type -> Type) -> Type -> Type -> k -> Type
data Actor m i o a = Spawn 
    { receive :: i -> m Unit
    , send :: o -> m Unit
    }