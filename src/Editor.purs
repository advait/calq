module Editor where

import Prelude
import ContentEditable as ContentEditable
import Data.Maybe (fromMaybe)
import React.Basic.Classic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events as Events
import Utils (debugLogAlt)

component :: React.Component Props
component = React.createComponent "Counter"

type Props
  = { label :: String
    }

counter :: Props -> React.JSX
counter =
  React.make component
    { initialState: { counter: 0, html: "1 + 1" }
    , render:
        \self ->
          R.div
            { children:
                [ R.button
                    { onClick: Events.capture_ $ self.setState (\s -> s { counter = s.counter + 1 })
                    , children: [ R.text (self.props.label <> ": " <> show self.state.counter) ]
                    }
                , ContentEditable.component
                    { html: self.state.html
                    , onChange: \e -> self.setState (\s -> s { counter = s.counter + 1, html = "<undefined>" `fromMaybe` e })
                    }
                ]
            }
    }
