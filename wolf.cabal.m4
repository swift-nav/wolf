name:                  wolf
version:               VERSION
synopsis:              Amazon Simple Workflow Service Wrapper.
description:           Wolf is a wrapper around Amazon Simple Workflow Service.
homepage:              https://github.com/swift-nav/wolf
license:               MIT
license-file:          LICENSE
author:                Swift Navigation Inc.
maintainer:            Mark Fine <dev@swiftnav.com>
copyright:             Copyright (C) 2015-2016 Swift Navigation, Inc.
category:              Network, AWS, Cloud, Distributed Computing
build-type:            Simple
cabal-version:         >= 1.22

source-repository head
  type:                git
  location:            git@github.com:swift-nav/wolf.git

library
  exposed-modules:     Network.AWS.Wolf
  other-modules:       Network.AWS.Wolf.Act
                     , Network.AWS.Wolf.Count
                     , Network.AWS.Wolf.Ctx
                     , Network.AWS.Wolf.Decide
                     , Network.AWS.Wolf.File
                     , Network.AWS.Wolf.Prelude
                     , Network.AWS.Wolf.SWF
                     , Network.AWS.Wolf.Types
                     , Network.AWS.Wolf.Types.Ctx
                     , Network.AWS.Wolf.Types.Product
                     , Network.AWS.Wolf.Types.Sum
  default-language:    Haskell2010
  hs-source-dirs:      src
  ghc-options:         -Wall
  build-depends:       aeson
                     , amazonka
                     , amazonka-swf
                     , base >= 4.8 && < 5
                     , bytestring
                     , conduit
                     , directory
                     , exceptions
                     , filepath
                     , http-types
                     , lifted-async
                     , lifted-base
                     , preamble
                     , process
                     , time
                     , transformers-base
                     , unliftio-core
                     , uuid
                     , yaml

executable wolf-actor
  hs-source-dirs:      main
  main-is:             actor.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , wolf
                     , optparse-generic
  default-language:    Haskell2010

executable wolf-decider
  hs-source-dirs:      main
  main-is:             decider.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , wolf
                     , optparse-generic
  default-language:    Haskell2010

executable wolf-counter
  hs-source-dirs:      main
  main-is:             counter.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , wolf
                     , optparse-generic
  default-language:    Haskell2010
