module Main where

import Foreign (Foreign)
import Lambda (Context)

newtype Response = Response
  { statusCode :: Int
  , isBase64Encoded :: Boolean
  , body :: String
  }

handler :: Context -> Foreign -> Response
handler _ _ =
  Response
  { statusCode: 200
  , isBase64Encoded: false
  , body: "Hello sailor!"
  }
