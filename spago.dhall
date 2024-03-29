{ name = "calq"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "group"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "quickcheck"
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
, sources = [ "engine/src/**/*.purs", "engine/test/**/*.purs" ]
}
