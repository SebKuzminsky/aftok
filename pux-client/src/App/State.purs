module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)
import Data.Maybe
import Prelude (($))

newtype State = State
  { title :: String
  , view :: ViewState
  , credentials :: Maybe Credentials
  }

data ViewState
  = Login LoginState
  | NotFound Route

type LoginState = 
  { credentials :: Credentials
  , error :: Maybe String
  }

type Credentials = 
  { username :: String
  , password :: String
  }

creds0 :: Credentials
creds0 = { username: "", password: "" }

login0 :: LoginState
login0 = { credentials: creds0 , error: Nothing }

init :: String -> State
init url = State
  { title: config.title
  , view:  Login login0
  , credentials: Nothing
  }
