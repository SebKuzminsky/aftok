module App.View.Login where

import Prelude ((<<<), const)
import App.Events (Event(..))
import App.State (LoginState)
import Control.Bind (discard)
import Data.Function (($))
-- import Data.Semigroup ((<>))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onInput, onClick, targetValue)
import Text.Smolder.HTML (input, label, form, button)
import Text.Smolder.HTML.Attributes (for, id, type', value)
import Text.Smolder.Markup ((!), (#!), text)

-- import Debug.Trace (trace)

loginForm :: LoginState -> HTML Event
loginForm ls = 
  form do
    label ! for "login-username" $ text "Username:"
    input ! type' "text" ! id "login-username" ! value ls.credentials.username #! onInput (UsernameChange <<< targetValue)

    label ! for "login-password" $ text "Password:"
    input ! type' "password" ! id "login-password" ! value ls.credentials.password #! onInput (PasswordChange <<< targetValue)

    button #! onClick (const SignIn) $ text "Sign In" 
