{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "calq"
, dependencies =
  [ "arrays"
  , "bignumber"
  , "console"
  , "effect"
  , "errors"
  , "group"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "psci-support"
  , "quickcheck"
  , "sorted-arrays"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
