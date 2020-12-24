module App.Editor where

import Prelude
import Bulma.Columns.Columns as Columns
import Bulma.Columns.Size (PercentSize(..))
import Bulma.Columns.Size as Columns.Size
import Bulma.Common as Bulma
import Bulma.Layout.Layout as Layout
import DOM.HTML.Indexed (GlobalProperties)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Debug.Trace (trace)
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (debugLogAlt, eventTargetTextContent, refireEvent)
import Web.Event.Event (Event, EventType(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type State
  = { lines :: Array String }

data Action
  = Increment
  | Change Event

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { lines: [] }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.id_ "editor-container", bulmaClasses [ Layout.container ] ]
    [ HH.div [ bulmaClasses [ Columns.columns ] ]
        [ HH.div
            [ HP.id_ "editor-left"
            , bulmaClasses [ Columns.column, Columns.Size.isPercentSize TwoThirds ]
            , HP.attr (AttrName "contenteditable") "true"
            , HE.handler (EventType "input") (\e -> refireEvent e $ Nothing)
            , HE.handler (EventType "delayedinput") (\e -> debugLogAlt e $ Just $ Change e)
            -- , HE.onKeyUp (\e -> Just $ Change (e))
            -- , HE.handler (EventType "keydown") (\e -> Just $ Change (e))
            ]
            [ HH.text $ String.joinWith "\n" state.lines ]
        , HH.div
            [ HP.id_ "results", bulmaClasses [ Columns.column ] ]
            [ HH.p_
                [ HH.text $ "You clicked " <> show state <> " times" ]
            , HH.button
                [ HE.onClick \_ -> Just Increment ]
                [ HH.text "Click me" ]
            ]
        ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction Increment = debugLogAlt "inc" (H.modify_ identity)

handleAction (Change e) = H.put { lines: String.split (Pattern "\n") $ eventTargetTextContent e }

bulmaClasses :: forall r i. Array (Bulma.ClassName) -> IProp (GlobalProperties r) i
bulmaClasses a = HP.classes $ convert <$> a
  where
  convert (Bulma.ClassName name) = H.ClassName name
