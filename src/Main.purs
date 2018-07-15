module Main where

import Prelude
import Foreign (Foreign, ForeignError)
import Effect (Effect)
import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Data.List.Types (NonEmptyList)

import Lambda (Context, Response(..), fail, succeed)
import APIGateway (Event(..), readEvent)
import Slack (decodeSlackCommand)

handler :: Context -> Foreign -> Effect Unit
handler ctx evt = do
  createResponse $ runExcept $ readEvent evt
  pure unit

  where
    createResponse :: Either (NonEmptyList ForeignError) Event -> Effect Unit
    createResponse (Left err) = fail ctx $ Response { statusCode: 502
                                                    , isBase64Encoded: false
                                                    , body: "Something went wrong!"
                                                    }
    createResponse (Right (Event res)) = succeed ctx $ Response { statusCode: 200
                                                        , isBase64Encoded: false
                                                        , body: "*Hello sailor!*\nYour request was: '" <> show (decodeSlackCommand res.body) <> "'"
                                                        }

