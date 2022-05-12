module FFI.NanoID where



import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import customAlphabetImpl :: EffectFn2 String Int String


customAlphabet :: String -> Int -> Effect String
customAlphabet = runEffectFn2 customAlphabetImpl