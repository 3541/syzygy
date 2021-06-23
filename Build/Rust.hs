module Build.Rust where

import Control.Exception.Extra
import Data.List
import Development.Shake
import Development.Shake.FilePath
import System.Directory

cargoEnv :: String -> [CmdOption]
cargoEnv buildDir =
  [ AddEnv "CARGO_TARGET_DIR" buildDir,
    AddEnv "RUSTFLAGS" "-Cforce-frame-pointers=yes -Zsymbol-mangling-version=v0"
  ]

rustTargetPath :: String -> CmdOption
rustTargetPath = AddEnv "RUST_TARGET_PATH"

rustVersion :: String -> Rules ()
rustVersion buildDir = do
  buildDir </> "rustc.version" %> \out -> do
    alwaysRerun
    Stdout stdout <- cmd "rustc --version"
    writeFileChanged out stdout

cargo :: Partial => String -> [CmdOption] -> String -> String -> [String] -> Action ()
cargo buildDir options operation proj args = do
  currentDir <- liftIO getCurrentDirectory
  need [buildDir </> "rustc.version", currentDir </> "Cargo.lock"]
  command_
    (cargoEnv buildDir ++ options)
    "cargo"
    $ [operation, "-p", proj] ++ args

cargoBuild :: Partial => String -> String -> String -> [String] -> [String] -> Action ()
cargoBuild buildDir targetSpec proj features args = do
  currentDir <- liftIO getCurrentDirectory
  let targetPath = currentDir </> "targets"
  let featureArgs =
        if not (null features)
          then ["--features", intercalate "," features]
          else []
  cargo buildDir [rustTargetPath targetPath] "build" proj $
    concat [["--target", targetSpec], featureArgs, args]

cargoTest :: Partial => String -> String -> [String] -> Action ()
cargoTest buildDir proj args = do
  cargo buildDir [] "test" proj args

-- Warning: Slow!
cargoMiriTest :: Partial => String -> String -> [String] -> Action ()
cargoMiriTest buildDir proj args = do
  cargo buildDir [] "clean" proj []
  command_
    (cargoEnv buildDir ++ [AddEnv "MIRIFLAGS" "-Zmiri-disable-stacked-borrows"])
    "cargo"
    $ ["miri", "test", "-p", proj] ++ args

cargoDoc :: Partial => String -> String -> [String] -> Action ()
cargoDoc buildDir proj args = do
  cargo buildDir [] "doc" proj (["--open", "--all-features"] ++ args)

cargoClippy :: Partial => String -> String -> String -> [String] -> Action ()
cargoClippy buildDir targetSpec proj args = do
  currentDir <- liftIO getCurrentDirectory
  cargo
    buildDir
    [rustTargetPath $ currentDir </> "targets"]
    "clippy"
    proj
    (["--target", targetSpec, "--all-features"] ++ args)
