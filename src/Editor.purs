module Editor where

import Prelude
import React.Basic.Classic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events as Events

component :: React.Component Props
component = React.createComponent "Counter"

type Props
  = { label :: String
    }

type State
  = { counter :: Int }

counter :: Props -> React.JSX
counter =
  React.make component
    { initialState: { counter: 0 } :: State
    , render:
        \self ->
          R.button
            { onClick: Events.capture_ $ self.setState (\s -> { counter: s.counter + 1 })
            , children: [ R.text (self.props.label <> ": " <> show self.state.counter) ]
            }
    }
