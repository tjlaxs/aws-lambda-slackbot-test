module APIGateway where

import Prelude
import Foreign (F, Foreign, readBoolean, readString)
import Foreign.Index ((!))

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
