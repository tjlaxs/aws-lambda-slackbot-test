module Main where

import Prelude (Unit, ($), discard, pure, unit)
import Foreign (Foreign)
import Lambda (Context, Response(..), succeed)
import Effect (Effect)
import Effect.Console (log)

handler :: Context -> Foreign -> Effect Unit
handler ctx _ = do
  succeed ctx $ Response { statusCode: 200
                         , isBase64Encoded: false
                         , body: "Hello sailor!"
                         }
  pure unit
