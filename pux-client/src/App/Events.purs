module App.Events where

import App.Routes as R
import App.State
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event
  = PageView R.Route
  | UsernameChange String
  | PasswordChange String
  | SignIn

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView R.Login) (State st) =
  noEffects $ State st { view = Login login0 }

foldp (UsernameChange n) (State (st @ { view: Login s })) =
  let
    updateCreds c = c { username = n }
    loginView = Login $ s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = loginView }

foldp (PasswordChange p) (State (st @ { view: Login s })) =
  let
    updateCreds c = c { password = p }
    loginView = Login $ s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = loginView }

foldp x y = noEffects y
