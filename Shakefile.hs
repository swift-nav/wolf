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

  -- | Cabal rules.
  --
  cabalRules "wolf.cabal"

  -- | Stack rules.
  --
  stackRules pats

  -- | sanity
  --
  fake' pats "sanity" $ const $
    need [ "lint", fakeFile "build-error" ]

  -- | Default things to run.
  --
  want [ "build-error", "lint", "format" ]
