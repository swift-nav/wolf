#!/usr/bin/env stack
{- stack
    runghc
    --package shakers
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import Development.Shakers

-- | Docker rules
--
dockerRules' :: Rules ()
dockerRules' = do
  let pats =
        [ "Dockerfile"
        , "Shakefile.hs"
        , "stack.yaml"
        , "shakers.cabal"
        , "src//*.hs"
        , "Setup.hs"
        , "LICENSE"
        ]

  -- | Docker rules.
  --
  dockerRules "." pats

  -- | Travis rule
  --
  phony "docker:travis" $ do
    need [ "mirrored", "docker:logined" ]
    xdocker_ [ "build", "-t", "travis", "." ]
    xdocker_ [ "run", "-t", "travis", "./Shakefile.hs" ]

-- | Main entry point.
--
main :: IO ()
main = shakeMain $ do
  let pats =
        [ "stack.yaml"
        , "Shakefile.hs"
        , "main//*.hs"
        , "src//*.hs"
        ]
      pats' = delete "stack.yaml" pats

  -- | Haskell rules.
  --
  hsRules "." pats'

  -- | Cabal rules.
  --
  cabalRules "." "wolf.cabal"

  -- | Stack rules.
  --
  stackRules "." pats

  -- | Docker rules.
  --
  dockerRules'

  -- | Default things to run.
  --
  want [ "build-error", "lint" ]
