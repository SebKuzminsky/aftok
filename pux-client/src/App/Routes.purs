module App.Routes where

import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Control.Apply ((<*))

import Pux.Router (end, router, lit)

data Route = Login | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Login <$ (lit "login") <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Login) = "/login"
