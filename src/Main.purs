module Main where

import Prelude (Unit, unit, pure, discard)
import Foreign (Foreign)
import Effect (Effect)
import Lambda (Context, succeed)

handler :: Context -> Foreign -> Effect Unit
handler cxt _ = do
  succeed cxt "Hello sailor!"
  pure unit
