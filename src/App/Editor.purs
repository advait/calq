module App.Editor where

import Prelude
import Bulma.Columns.Columns as Columns
import Bulma.Columns.Size (PercentSize(..))
import Bulma.Columns.Size as Columns.Size
import Bulma.Common as Bulma
import Bulma.Layout.Layout as Layout
import DOM.HTML.Indexed (GlobalProperties)
import Data.Maybe (Maybe(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State
  = { count :: Int }

data Action
  = Increment

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
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
            ]
            [ HH.text "Hello world" ]
        , HH.div
            [ HP.id_ "results", bulmaClasses [ Columns.column ] ]
            [ HH.p_
                [ HH.text $ "You clicked " <> show state.count <> " times" ]
            , HH.button
                [ HE.onClick \_ -> Just Increment ]
                [ HH.text "Click me" ]
            ]
        ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }

bulmaClasses :: forall r i. Array (Bulma.ClassName) -> IProp (GlobalProperties r) i
bulmaClasses a = HP.classes $ convert <$> a
  where
  convert (Bulma.ClassName name) = H.ClassName name
