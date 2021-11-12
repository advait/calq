{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "calq"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "functions"
  , "group"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "react-basic"
  , "react-basic-dom"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "transformers"
  , "tuples"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
