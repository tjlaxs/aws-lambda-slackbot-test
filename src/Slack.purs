module Slack where

import Prelude (class Show, ($), (<<<), map)
import Data.Array (head, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeDecodeURIComponent)

newtype Token = Token String
instance showToken :: Show Token where
  show _ = "Token (censored)"

newtype Team = Team
  { id :: String
  , domain :: String
  }

newtype Channel = Channel
  { id :: String
  , name :: String
  }

newtype User = User
  { id :: String
  , name :: String
  }

newtype CommandRequest = CommandRequest
  { token :: Token
  , team :: Team
  , channel :: Channel
  , user :: User
  , command :: String
  , text :: String
  , responseUrl :: String
  , triggerId :: String
  }

decodeSlackCommand :: String -> Array (Tuple String (Maybe String))
decodeSlackCommand =
  map tuplify <<< map (map unsafeDecodeURIComponent) <<< map splitKeyvals <<< splitParams

  where
    tuplify :: Array String -> Tuple String (Maybe String)
    tuplify xs = Tuple (fromMaybe "" $ head xs) (xs !! 1)

    splitKeyvals :: String -> (Array String)
    splitKeyvals = split (Pattern "=")

    splitParams :: String -> Array String
    splitParams = split (Pattern "&")
