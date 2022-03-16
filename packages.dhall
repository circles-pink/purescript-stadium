let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220203/packages.dhall sha256:f8905bf5d7ce9d886cf4ef1c5893ab55de0b30c82c2b4137f272d075000fbc50

in  upstream
  with dot-language =
    { dependencies =
      [ "arrays"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "prelude"
      , "psci-support"
      , "test-unit"
      , "tuples"
      , "type-equality"
      , "typelevel-lists"
      , "typelevel-prelude"
      , "undefined"
      , "unsafe-coerce"
      , "variant"
      ]
    , repo = "https://github.com/thought2/purescript-dot-language.git"
    , version = "76abe9daa5370214f2733f45bd277d59a98e890c"
    }
