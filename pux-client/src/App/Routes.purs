module App.Routes where

import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router)

data Route = Login | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Login <$ end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Login) = "/login"
