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

  -- | Haskell rules.
  --
  hsRules "."

  -- | Cabal rules.
  --
  cabalRules "." "wolf.cabal"

  -- | Stack rules.
  --
  stackRules "." pats

  -- | sanity
  --
  fake "." pats "sanity" $ const $
    need [ fakeFile "build-error", fakeFile "lint" ]

  -- | Default things to run.
  --
  want [ fakeFile "sanity", fakeFile "format" ]
