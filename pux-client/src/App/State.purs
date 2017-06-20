module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)
import Data.Maybe
import Prelude (($))

newtype State = State
  { title :: String
  , route :: Route
  , view :: ViewState
  , credentials :: Maybe Credentials
  }

data ViewState
  = Login LoginState

newtype LoginState = LoginState
  { credentials :: Credentials
  , error :: Maybe String
  }

newtype Credentials = Credentials
  { username :: String
  , password :: String
  }

emptyCreds :: Credentials
emptyCreds = Credentials { username: "", password: "" }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , view:  Login $ LoginState { credentials: emptyCreds , error: Nothing }
  , credentials: Nothing
  }
