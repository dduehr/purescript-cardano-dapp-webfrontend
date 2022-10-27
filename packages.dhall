{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
--let upstream =
--      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221018/packages.dhall
--        sha256:b1db2e4a17260ace8d17858602f8c56f460982d6e404818d7f6cb9f053324bb1
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220504/packages.dhall
        sha256:fd37736ecaa24491c907af6a6422156417f21fbf25763de19f65bd641e8340d3
in  upstream
{-
  with purescript-cip30 =
      { dependencies = 
          [ "console"
          , "effect"
          , "prelude"
          , "aff"
          , "aff-promise" 
          , "arrays"
          , "maybe"
          , "nullable"    
          ]
      , repo =
          "https://github.com/anton-k/purescript-cip30.git"
      , version =
          "841c8f83645a833e0ce4d5853a8288b296f7c101"
      }
-}
  with purescript-cip30 = ../purescript-cip30/spago.dhall as Location
  with purescript-cardano-serialization-lib =
      { dependencies = 
          [ "arraybuffer-types"
          , "console"
          , "effect"
          , "either"
          , "maybe"
          , "nullable"
          , "partial"
          , "prelude"
          , "foldable-traversable"
          , "argonaut-core"
          ] 
      , repo =
          "https://github.com/anton-k/purescript-cardano-serialization-lib"
      , version =
          "541eb0b1bf128a426d3d0d1e8ffc683a1237de23"
      }