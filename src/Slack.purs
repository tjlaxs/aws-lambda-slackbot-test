module Slack where

import Prelude (($), (<<<), map)
import Data.Array (head, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Pattern (Pattern(..))
import Data.String.Common (split)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeDecodeURIComponent)

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
