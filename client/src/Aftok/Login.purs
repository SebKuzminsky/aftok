module Aftok.Login where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Maybe (Maybe(..))
import Data.Functor (($>))

import Halogen
import Halogen.HTML.Core (className)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), affjax)
import Network.HTTP.Method 
import Network.HTTP.StatusCode 

type LoginState = { username :: String, password :: String }

initialState :: LoginState
initialState = { username: "", password: "" }

-- | The component query algebra.
data LoginAction a
  = SetUsername String a
  | SetPassword String a
  | Login String String a

-- | The effects used in the login component.
type LoginEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component LoginState LoginAction (Aff (LoginEffects eff))
ui = component render eval
  where

  render :: LoginState -> ComponentHTML LoginAction
  render st =
    H.div 
      [ P.classes (className <$> ["panel", "panel-primary"]) ]
      [ H.div 
        [ P.classes [ className "panel-heading" ] ]
        [ H.h3 [ P.classes [ className "panel-title" ]] [ H.text "Aftok Login" ] ]
      , H.div 
        [ P.classes [ className "panel-body" ] ]
        [
          H.h2_
            [ H.text "username:" ]
        , H.p_
            [ H.input
                [ P.value st.username
                , P.inputType P.InputText
                , E.onValueInput (E.input SetUsername)
                ]
            ]
        , H.h2_
            [ H.text "password:" ]
        , H.p_
            [ H.input
                [ P.value st.password
                , P.inputType P.InputPassword
                , E.onValueInput (E.input SetPassword)
                ]
            ]
        , H.p_
            [ H.button
                [ P.classes (className <$> ["btn", "btn-primary"])
                , E.onClick (E.input_ (Login st.username st.password)) 
                ]
                [ H.text "Login" ]
            ]
        ]
      ]

  eval :: Natural LoginAction (ComponentDSL LoginState LoginAction (Aff (LoginEffects eff)))
  eval (SetUsername user next) = modify (_ { username = user }) $> next
  eval (SetPassword pass next) = modify (_ { password = pass }) $> next
  eval (Login user pass next) = do
    result <- liftAff' (login user pass)
    pure next

data LoginResponse 
  = OK 
  | Forbidden 
  | Error { status :: StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: forall eff. String -> String -> Aff (ajax :: AJAX | eff) LoginResponse
login user pass = do
  result <- affjax $ { method: POST
                     , url: "/login"
                     , headers: []
                     , content: Nothing :: Maybe String
                     , username: Just user
                     , password: Just pass
                     }
  pure $ case result.status of 
    StatusCode 403 -> Forbidden
    StatusCode 200 -> OK
    other -> Error { status: other , message: result.response }

