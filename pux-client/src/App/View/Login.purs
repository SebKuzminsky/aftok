module App.View.Login where

import App.Events (Event(..))
import App.State
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onInput)
import Text.Smolder.HTML (a, div, h1, input, label, form, button)
import Text.Smolder.HTML.Attributes (className, for, href, id, type', value)
import Text.Smolder.Markup ((!), (#!), text)

loginForm :: LoginState -> HTML Event
loginForm loginstate =
  let
    username :: String
    username = "franco"

    password :: String
    password = "SecrETP@ssword"
  in
    form do
      label ! for "login-username" $ text "Username:"
      input ! value username ! type' "text" ! id "login-username" #! onInput UsernameChange

      label ! for "login-password" $ text "Password:"
      input ! value password ! type' "password" ! id "login-password" #! onInput PasswordChange

      button $ text "Submit"
