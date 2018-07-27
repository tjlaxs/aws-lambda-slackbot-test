module Slack where

import Prelude (class Show, ($), (<<<), (<>), map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (fromMaybe)
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeDecodeURIComponent)

newtype Token = Token String
instance showToken :: Show Token where
  show _ = "Token (censored)"

data Team = Team String String -- Id Domain
instance showTeam :: Show Team where
  show (Team i d) = "Team " <> i <> " " <> d

data Channel = Channel String String -- Id Name
instance showChannel :: Show Channel where
  show (Channel i n) = "Team " <> i <> " " <> n

data User = User String String -- Id Name
instance showUser :: Show User where
  show (User i n) = "User " <> i <> " " <> n

data Command = Command String String -- Command Text
instance showCommand :: Show Command where
  show (Command c t) = "Command " <> c <> " " <> t

newtype CommandRequest = CommandRequest
  { token :: Token
  , team :: Team
  , channel :: Channel
  , user :: User
  , command :: Command
  , responseUrl :: String
  , triggerId :: String
  }
derive instance genericCommandRequest :: Generic CommandRequest _
instance showCommandRequest :: Show CommandRequest where
  show = genericShow

decodeSlackCommand :: String -> CommandRequest
decodeSlackCommand =
  mkCommandRequest
    <<< map tuplify
    <<< map (map unsafeDecodeURIComponent)
    <<< map splitKeyvals
    <<< splitParams
  --map tuplify <<< map (map unsafeDecodeURIComponent) <<< map splitKeyvals <<< splitParams

  --where
    --tuplify :: Array String -> Tuple String (Maybe String)
    --tuplify xs = Tuple (fromMaybe "" $ head xs) (xs !! 1)

tuplify :: Array String -> Tuple String String
tuplify [x,y] = Tuple x y
tuplify [x] = Tuple x ""
tuplify _ = Tuple "" "" -- FIXME: ugly

mkCommandRequest :: Array (Tuple String String) -> CommandRequest
mkCommandRequest xs = CommandRequest
  { token : Token (f "token")
  , team : Team (f "team_id") (f "team_domain")
  , channel : Channel (f "channel_id") (f "channel_name")
  , user : User (f "user_id") (f "user_name")
  , command : Command (f "command") (f "text")
  , responseUrl : f "response_url"
  , triggerId : f "trigger_id"
  }
    where
      ms = Map.fromFoldable xs
      f x = fromMaybe "" $ Map.lookup x ms

splitKeyvals :: String -> Array String
splitKeyvals =  split (Pattern "=")

splitParams :: String -> Array String
splitParams = split (Pattern "&")
