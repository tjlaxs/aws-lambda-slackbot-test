module Main where

import Prelude
import Data.Array (head, (!!))
import Foreign (F, Foreign, ForeignError, readBoolean, readString)
import Foreign.Index ((!))
import Lambda (Context, Response(..), fail, succeed)
import Effect (Effect)
import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Data.List.Types (NonEmptyList)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Global.Unsafe (unsafeDecodeURI)

newtype Event = Event
  { resource :: String
  , path :: String
  , httpMethod :: String
  , body :: String
  , isBase64Encoded :: Boolean
  }

readEvent :: Foreign -> F Event
readEvent value = do
  resource <- value ! "resource" >>= readString
  path <- value ! "path" >>= readString
  httpMethod <- value ! "httpMethod" >>= readString
  body <- value ! "body" >>= readString
  isBase64Encoded <- value ! "isBase64Encoded" >>= readBoolean
  pure $ Event { resource, path, httpMethod, body, isBase64Encoded }

tuplify :: Array String -> Tuple String (Maybe String)
tuplify xs = Tuple (fromMaybe "" $ head xs) (xs !! 1)

splitKeyvals :: String -> (Array String)
splitKeyvals = split (Pattern "=")

splitParams :: String -> Array String
splitParams = split (Pattern "&")

decodeSlackCommand :: String -> Array (Tuple String (Maybe String))
decodeSlackCommand = map tuplify <<< map (map unsafeDecodeURI) <<< map splitKeyvals <<< splitParams

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

