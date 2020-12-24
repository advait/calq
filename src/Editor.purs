module Editor where

import Prelude
import ContentEditable as ContentEditable
import Data.Maybe (fromMaybe, Maybe(..))
import React.Basic.Classic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events as Events

component :: React.Component Props
component = React.createComponent "Editor"

type Props
  = { initialValue :: String
    , className :: String
    }

counter :: Props -> React.JSX
counter =
  React.make component
    { initialState: { counter: 0, html: Nothing }
    , render:
        \self ->
          R.div
            { className: "editor " <> self.props.className
            , children:
                [ R.div
                    { className: "columns"
                    , children:
                        [ ContentEditable.component
                            { html: self.props.initialValue `fromMaybe` self.state.html
                            , onChange: \e -> self.setState (\s -> s { counter = s.counter + 1, html = e })
                            , className: "editor-left column is-two-thirds"
                            }
                        , R.div
                            { onClick: Events.capture_ $ self.setState (\s -> s { counter = s.counter + 1 })
                            , children: [ R.text (self.props.initialValue <> ": " <> show self.state.counter) ]
                            , className: "editor-right column"
                            }
                        ]
                    }
                ]
            }
    }
