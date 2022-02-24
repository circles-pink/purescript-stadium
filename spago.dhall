{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "stadium"
, dependencies =
  [ "arrays"
  , "console"
  , "dotlang"
  , "effect"
  , "foldable-traversable"
  , "identity"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
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
  ]
, packages = ./packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs", "./examples/**/*.purs" ]
}
