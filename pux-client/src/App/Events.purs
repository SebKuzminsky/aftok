module App.Events where

import App.Routes as R
import App.State
import Data.Function (($))
import Data.Semigroup ((<>))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

-- import Debug.Trace (trace)

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
    viewState = s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = Login viewState }

foldp (PasswordChange p) (State (st @ { view: Login s })) =
  let
    updateCreds c = c { password = p }
    viewState = s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = Login viewState }

foldp x y = noEffects y
