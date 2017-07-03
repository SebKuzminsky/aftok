module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Newtype (class Newtype)
import Data.Maybe
import Prelude (($))

newtype State = State
  { view :: ViewState
  }

data ViewState
  = Login LoginState
  | Home
  | NotFound Route

type LoginState = 
  { credentials :: Credentials
  , submitted :: Boolean
  , error :: Maybe String
  }

type Credentials = 
  { username :: String
  , password :: String
  }

creds0 :: Credentials
creds0 = { username: "", password: "" }

login0 :: LoginState
login0 = { credentials: creds0, submitted: false, error: Nothing }

type HomeState = 
  { title :: String }

init :: String -> State
init url = State
  { view: Login login0
  }
