module Lambda where

import Prelude
import Effect (Effect)

newtype Response = Response
  { statusCode :: Int
  , isBase64Encoded :: Boolean
  , body :: String
  }

foreign import data Context :: Type

foreign import succeed :: Context -> Response -> Effect Unit

foreign import fail :: Context -> Response -> Effect Unit
