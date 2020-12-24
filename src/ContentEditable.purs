module ContentEditable where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic.Classic as React
import React.Basic.DOM.Events as Events
import React.Basic.Events (EventHandler, handler)

contentEditableComponent :: React.Component Props
contentEditableComponent = React.createComponent "ContentEditable"

type Props
  = { html :: String
    , onChange :: Maybe String -> Effect Unit
    }

component :: Props -> React.JSX
component props =
  React.make contentEditableComponent
    { render
    , initialState: unit
    }
    props

render :: React.Self Props Unit -> React.JSX
render self@{ state, props } =
  contentEditableInternal
    { html: props.html
    , onChange: handler Events.targetValue props.onChange
    }

type ContentEditableInternalProps
  = { html :: String
    , onChange :: EventHandler
    }

foreign import contentEditableInternal :: ContentEditableInternalProps -> React.JSX
