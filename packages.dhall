let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220203/packages.dhall
        sha256:f8905bf5d7ce9d886cf4ef1c5893ab55de0b30c82c2b4137f272d075000fbc50

let additions =
      { dotlang =
        { dependencies =
          [ "arrays"
          , "colors"
          , "console"
          , "effect"
          , "maybe"
          , "prelude"
          , "psci-support"
          , "strings"
          ]
        , repo = "https://github.com/csicar/purescript-dotlang.git"
        , version = "v4.0.0"
        }
      }

in  upstream // additions
