module Lambda where

import Prelude
import Effect (Effect)

foreign import data Context :: Type

foreign import succeed :: Context -> String -> Effect Unit

foreign import fail :: Context -> String -> Effect Unit
