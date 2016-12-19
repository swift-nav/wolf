#!/usr/bin/env stack
{- stack
    runghc
    --package basic-prelude
    --package directory
    --package shake
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import BasicPrelude
import Data.Char
import Development.Shake
import Development.Shake.FilePath
import System.Directory

-- | This file used for version change detection.
--
thisFile :: FilePath
thisFile = "Shakefile.hs"

-- | Location of build supporting files.
--
buildDir :: FilePath
buildDir = ".build"

-- | Location of stack's work files.
--
stackDir :: FilePath
stackDir = ".stack-work"

-- | Build directory where "touch" files are kept.
--
fakeDir :: FilePath
fakeDir = buildDir </> "fake"

-- | Build directory where docker files are kept.
--
dockerDir :: Action FilePath
dockerDir = do
  dir <- liftIO getCurrentDirectory
  return $ buildDir </> takeFileName dir

-- | Fake directory path builder.
--
fd :: FilePath -> FilePath
fd = (fakeDir </>)

-- | Remove right excess on string.
--
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

-- | Typeful command args with return string.
--
cmdArgs :: String -> [String] -> Action String
cmdArgs c as = rstrip . fromStdout <$> cmd c as

-- | Typeful command args with no return.
--
cmdArgs_ :: String -> [String] -> Action ()
cmdArgs_ c as = unit $ cmd c as

-- | Run commands in a dir with return string.
--
_cmdArgsDir :: FilePath -> String -> [String] -> Action String
_cmdArgsDir d c as = rstrip . fromStdout <$> cmd (Cwd d) c as

-- | Run commands in a dir with no return.
--
cmdArgsDir_ :: FilePath -> String -> [String] -> Action ()
cmdArgsDir_ d c as = unit $ cmd (Cwd d) c as

-- | Run docker command in docker dir.
--
docker :: [String] -> Action ()
docker args = do
  dir <- dockerDir
  cmdArgsDir_ dir "docker" args

-- | Stack command.
--
stack :: [String] -> Action ()
stack = cmdArgs_ "stack"

-- | Stack exec command.
--
_stackExec :: String -> [String] -> Action ()
_stackExec cmd' args = stack $ "exec" : cmd' : "--" : args

-- | Sylish command.
--
stylish :: [String] -> Action ()
stylish = cmdArgs_ "stylish-haskell"

-- | Lint command.
--
lint :: [String] -> Action ()
lint = cmdArgs_ "hlint"

-- | Git command.
--
git :: [String] -> Action String
git = cmdArgs "git"

-- | m4 command.
--
m4 :: [String] -> Action String
m4 = cmdArgs "m4"

-- | Version.
--
version :: Action String
version = git [ "describe", "--tags", "--abbrev=0" ]

-- | Touch a file for fake files.
--
touchFile :: FilePath -> Action ()
touchFile = flip writeFile' mempty

-- | Copy a file if changed, creating parent directories.
--
copyFileChanged' :: FilePath -> FilePath -> Action ()
copyFileChanged' a b = do
  liftIO $ createDirectoryIfMissing True $ dropFileName b
  copyFileChanged a b

-- | Preprocess a file with m4
--
preprocess :: FilePattern -> FilePath -> Action [(String, String)] -> Rules ()
preprocess target file macros =
  target %> \out -> do
    need [ file ]
    let f k v = "-D" <> k <> "=" <> v
    macros' <- macros
    content <- m4 $ file : (uncurry f <$> macros')
    writeFileChanged out content

-- | Use a fake file to keep track of the last time an file-free action ran.
--
fake :: [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake pats target act = do
  fd target %> \out -> do
    files <- getDirectoryFiles "." pats
    need files
    act files
    touchFile out

  phony target $
    need [ fd target ]

-- | Global rules
--
globalRules :: Rules ()
globalRules = do
  let pats =
        [ "stack.yaml"
        , "Shakefile.hs"
        , "main//*.hs"
        , "src//*.hs"
        , "test//*.hs"
        ]

  -- | wolf.cabal
  --
  preprocess "wolf.cabal" "wolf.cabal.m4" $ do
    v <- version
    return [ ("VERSION", v) ]

  -- | build
  --
  fake pats "build" $ \_files -> do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast" ]

  -- | build-error
  --
  fake pats "build-error" $ \_files -> do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--ghc-options=-Werror" ]

  -- | build-tests
  --
  fake pats "build-tests" $ \_files -> do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--test", "--no-run-tests" ]

  -- | build-tests-error
  --
  fake pats "build-tests-error" $ \_files -> do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--test", "--no-run-tests", "--ghc-options=-Werror" ]

  -- | tests
  --
  phony "tests" $ do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--test" ]

  -- | tests-error
  --
  phony "tests-error" $ do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--test", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony "ghci" $ do
    need [ "wolf.cabal" ]
    stack [ "ghci", "--fast" ]

  -- | ghci-tests
  --
  phony "ghci-tests" $ do
    need [ "wolf.cabal" ]
    stack [ "ghci", "--fast", "--test" ]

  -- | install
  --
  fake pats "install" $ \_files -> do
    need [ "wolf.cabal" ]
    stack [ "build", "--fast", "--copy-bins" ]

  -- | publish
  --
  phony "publish" $ do
    need [ "wolf.cabal" ]
    stack [ "sdist" ]
    stack [ "upload", "." ]

  -- | clean
  --
  phony "clean" $ do
    need [ "wolf.cabal" ]
    stack [ "clean" ]
    removeFilesAfter buildDir [ "//*" ]

  -- | clear
  --
  phony "clear" $
    forM_ [ fakeDir ] $ \dir ->
      removeFilesAfter dir [ "//*" ]

  -- | wipe
  --
  phony "wipe" $ do
    removeFilesAfter buildDir [ "//*" ]
    removeFilesAfter stackDir [ "//*" ]

  -- | sanity
  --
  phony "sanity" $
    need [ "tests-error", "lint" ]

-- | Haskell source rules
--
hsRules :: Rules ()
hsRules = do
  let pats =
        [ "Shakefile.hs"
        , "main//*.hs"
        , "src//*.hs"
        , "test//*.hs"
        ]

  -- | format
  --
  fake pats "format" $ \files -> do
    need [ ".stylish-haskell.yaml" ]
    stylish $ [ "-c", ".stylish-haskell.yaml", "-i" ] <> files

  -- | lint
  --
  fake pats "lint" $ \files ->
    lint files

-- | Docker rules
--
dockerRules :: Rules ()
dockerRules = do
  let pats =
        [ "Dockerfile"
        , "Shakefile.hs"
        , ".dockerignore"
        , "stack.yaml"
        , "registrar.cabal"
        , "main//*.hs"
        , "src//*.hs"
        ]

  -- | docker:setup
  --
  fake pats "docker:setup" $ \files ->
    forM_ files $ \file -> do
      dir <- dockerDir
      copyFileChanged' file $ dir </> file

  -- | docker:build
  --
  phony "docker:build" $ do
    need [ "docker:setup" ]
    docker [ "build", "." ]

-- | Main entry point
--
main :: IO ()
main = do
  v <- getHashedShakeVersion [thisFile]
  shakeArgs shakeOptions { shakeFiles = buildDir, shakeVersion = v } $ do
    want [ "tests-error", "lint", "format" ]
    globalRules
    hsRules
    dockerRules
