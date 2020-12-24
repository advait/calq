module Editor where

import Prelude
import ContentEditable as ContentEditable
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, Maybe(..))
import Interpreter (evalProgram', showPretty)
import React.Basic.Classic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events as Events

component :: React.Component Props
component = React.createComponent "Editor"

type Props
  = { initialValue :: String
    , className :: String
    }

editor :: Props -> React.JSX
editor =
  React.make component
    { initialState: { counter: 0, html: Nothing }
    , render:
        \self ->
          let
            program = self.props.initialValue `fromMaybe` self.state.html

            output = case evalProgram' program of
              Left err -> show err
              Right val -> showPretty val
          in
            R.div
              { className: "editor " <> self.props.className
              , children:
                  [ R.div
                      { className: "columns"
                      , children:
                          [ ContentEditable.component
                              { html: program
                              , onChange: \e -> self.setState (\s -> s { counter = s.counter + 1, html = e })
                              , className: "editor-left column is-two-thirds"
                              }
                          , R.div
                              { onClick: Events.capture_ $ self.setState (\s -> s { counter = s.counter + 1 })
                              , children: [ R.text output ]
                              , className: "editor-right column"
                              }
                          ]
                      }
                  ]
              }
    }
