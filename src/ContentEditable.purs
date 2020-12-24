module ContentEditable (component, Props) where

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
    , className :: String
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
    , className: props.className
    }

type ContentEditableInternalProps
  = { html :: String
    , onChange :: EventHandler
    , className :: String
    }

foreign import contentEditableInternal :: ContentEditableInternalProps -> React.JSX
