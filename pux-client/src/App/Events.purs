module App.Events where

import App.Routes (Route)
import App.State
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event
  = PageView Route
  | UsernameChange String
  | PasswordChange String
  | SignIn

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
  noEffects $ State st { route = route }

foldp (UsernameChange evt) (State (st @ { view: Login (LoginState s) })) =
  let
    updateCreds (Credentials c) = Credentials $ c { username = evt }

    loginView :: ViewState
    loginView = Login $ LoginState s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = loginView }

foldp x y = noEffects y
