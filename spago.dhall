{ name = "stadium"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "identity"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-lists"
  , "typelevel-prelude"
  , "undefined"
  , "unsafe-coerce"
  , "variant"
  , "dot-language"
  ]
, packages = ./packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs", "./examples/**/*.purs" ]
}
