module App.Events where

import Prelude ((<<<), bind, show)
import App.Routes as R
import App.State
import Control.Applicative (pure)
import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (catchError)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show 
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Pux (EffModel, noEffects)

import Network.HTTP.Affjax (AJAX(), affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Debug.Trace (trace)

data Event
  = PageView R.Route
  | UsernameChange String
  | PasswordChange String
  | SignIn
  | SignInComplete LoginResponse

data LoginResponse 
  = OK 
  | Forbidden 
  | Error { status :: StatusCode, message :: String }

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView R.Login) (State st) =
  noEffects $ State st { view = Login login0 }

foldp (UsernameChange n) (State (st @ { view: Login s })) =
  trace "Username changed... " \_ -> 
  let
    updateCreds c = c { username = n }
    viewState = s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = Login viewState }

foldp (PasswordChange p) (State (st @ { view: Login s })) =
  trace "Password changed... " \_ -> 
  let
    updateCreds c = c { password = p }
    viewState = s { credentials = updateCreds s.credentials }
  in
    noEffects $ State st { view = Login viewState }
    
foldp SignIn (State st @ { view: (Login s) }) = 
  trace "Signing in... " \_ -> 
  { state: State { view: Login s { submitted = true } }
  , effects: [ (Just <<< SignInComplete) <$> catchError (login s.credentials) (\e -> pure $ Error { status: StatusCode 500, message: show e }) ]
  }

foldp (SignInComplete resp) (State st) =
  trace "Signin Complete... " \_ -> 
  case resp of
       OK ->        noEffects $ State st { view = Home }
       Forbidden -> noEffects $ State st { view = Login login0 { error = Just "Access denied." } }
       Error e ->   noEffects $ State st { view = Login login0 { error = Just e.message } }

foldp e y = 
  trace "Event not recognized." \_ ->
  noEffects y

-- | Post credentials to the login service and interpret the response
login :: forall eff. Credentials -> Aff (ajax :: AJAX | eff) LoginResponse
login c = do
  result <- affjax $ { method: Left POST
                     , url: "https://www.aftok.com/login"
                     , headers: []
                     , content: Nothing :: Maybe String
                     , username: Just c.username
                     , password: Just c.password
                     , withCredentials: true
                     }
  pure $ case result.status of 
    StatusCode 403 -> Forbidden
    StatusCode 200 -> OK
    other -> Error { status: other , message: result.response }
