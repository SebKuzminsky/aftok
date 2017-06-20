module App.View.Login where

import Prelude ((<<<))
import App.Events (Event(..))
import App.State
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onInput, DOMEvent, targetValue)
import Text.Smolder.HTML (a, div, h1, input, label, form, button)
import Text.Smolder.HTML.Attributes (className, for, href, id, type', value)
import Text.Smolder.Markup ((!), (#!), text)

loginForm :: Credentials -> HTML Event
loginForm (Credentials c) =
  form do
    label ! for "login-username" $ text "Username:"
    input ! value c.username ! type' "text" ! id "login-username" #! onInput (UsernameChange <<< targetValue)

    label ! for "login-password" $ text "Password:"
    input ! value c.password ! type' "password" ! id "login-password" #! onInput (PasswordChange <<< targetValue)

    button $ text "Submit"
