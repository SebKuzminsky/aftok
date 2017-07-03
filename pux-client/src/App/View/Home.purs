module App.View.Home where

import App.Events (Event)
import App.State (HomeState)
import Data.Function (($))

import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)


homeView :: HomeState -> HTML Event
homeView s = 
   h1 $ text s.title

