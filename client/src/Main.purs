module Main where

import Prelude 

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.State.Class (modify)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..), maybe)
import Data.UUID (UUID, parseUUID)
import Data.String (toLower)
import Data.Tuple (Tuple(..))

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff (HalogenEffects)
import Halogen.Component.ChildPath (ChildPath, cpR, cpL)
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax (AJAX())

import Aftok.Login as L
import Aftok.Timeline as T

type AppEffects eff = HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)

main :: Eff (AppEffects ()) Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  driver <- runUI ui unit body
  forkAff $ routeSignal driver

data Location
  = Login
  | Home
  | Project UUID
  | NotFound

routing :: Match Location
routing = Home <$ lit ""
      <|> Login <$ (lit "" *> lit "login")
      <|> (maybe NotFound Project <<< parseUUID) <$> (lit "" *> lit "project" *> str)

data Goto a
  = Goto Location a


type RoutingState =
  { currentLoc :: Location
  }

init :: RoutingState
init = { currentLoc: Login }


type ChildQuery = Coproduct L.LoginAction T.TimelineAction
type ChildSlot = Either L.Slot T.Slot

loginPath :: ChildPath L.LoginAction ChildQuery L.Slot ChildSlot
loginPath = cpL

timelinePath :: ChildPath T.TimelineAction ChildQuery T.Slot ChildSlot
timelinePath = cpR

type QueryP
  = Coproduct Goto ChildQuery

pageName :: Location -> String
pageName Home = "Home"
pageName Login = "Login"
pageName (Project _) = "Project"
pageName NotFound = "404"

ui :: forall eff. H.Component HH.HTML Goto Unit Void (Aff (AppEffects eff))
ui = H.parentComponent
  { initialState: const ({ currentLoc: Login })
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: RoutingState -> H.ParentHTML Goto ChildQuery ChildSlot (Aff (AppEffects eff))
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (pageName st.currentLoc) ]
        , HH.ul_ (map link [Login, Home])
        , viewPage st.currentLoc
        ]

    link s = HH.li_ [ HH.a [ HP.href ("#/" <> toLower (pageName s)) ] [ HH.text (pageName s) ] ]

    viewPage :: Location -> H.ParentHTML Goto ChildQuery ChildSlot (Aff (AppEffects eff))
    viewPage Login =
      HH.slot' loginPath L.Slot L.ui L.initialState (\L.LoginComplete -> Just (Goto Home unit))
    viewPage _ =
      HH.div_ []

    eval :: Goto ~> H.ParentDSL RoutingState Goto ChildQuery ChildSlot Void (Aff (AppEffects eff))
    eval (Goto loc next) = do
      modify (_ { currentLoc = loc })
      pure next

routeSignal :: forall eff. H.HalogenIO Goto Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Goto Void (Aff (HA.HalogenEffects eff))
          -> Maybe Location
          -> Location
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
