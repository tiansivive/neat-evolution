module Graphics.Animation
  ( Animation
  , Shared
  )
  where


import Prelude

import App.Graphics (DrawDependencies)
import Control.Monad.Reader (ReaderT)
import Creature (Creature)
import Creature as C
import Data.Actor (Actor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Ref (Ref)
import Habitat (Habitat)
import Type.Row (type (+))

type Shared r = ( creatures :: Ref (Array Creature), world :: Habitat | r )



type Animation = ReaderT ( Record (Shared + DrawDependencies ()) ) Effect 


data In s = Start | Stop | Pause | Play s


on :: In -> Animation Unit
on msg = pure unit


render :: forall m. Actor m (In (Array Creature)) Unit
render = do
    Play creatures <- ?receive
    
    for creatures C.draw
    

animation :: Actor Animation In Unit
animation = pure unit
  
  

-- render :: Animation Unit
-- render = do
--     { ctx, creatures, world } <- ask
--     for creatures C.draw
    


-- loop :: Animation Unit
-- loop = do
--   forever (lift2 $ requestAnimationFrame render)
--   requestAnimationFrame 