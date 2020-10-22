module Build.Rust where

import Control.Exception.Extra
import Data.List
import System.Directory

import Development.Shake
import Development.Shake.FilePath

cargoEnv :: String -> [CmdOption]
cargoEnv buildDir = [
  AddEnv "CARGO_TARGET_DIR" buildDir,
  AddEnv "RUSTFLAGS" "-Cforce-frame-pointers=yes -Zsymbol-mangling-version=v0"
  ]

rustVersion :: String -> Rules ()
rustVersion buildDir = do
  buildDir </> "rustc.version" %> \out -> do
    alwaysRerun
    Stdout stdout <- cmd "rustc --version"
    writeFileChanged out stdout

cargo :: Partial => String -> [CmdOption] -> String -> String -> [String] -> Action ()
cargo buildDir options operation proj args = do
  need [buildDir </> "rustc.version"]
  command_ (concat [cargoEnv buildDir, options])
    "cargo" $ concat [[operation, "-p", proj], args]

cargoBuild :: Partial => String -> String -> String -> [String] -> [String] -> Action ()
cargoBuild buildDir targetSpec proj features args = do
  currentDir <- liftIO getCurrentDirectory
  let targetPath = currentDir </> "targets"
  let featureArgs = if length features > 0
        then ["--features", intercalate "," features]
        else []
  cargo buildDir [AddEnv "RUST_TARGET_PATH" targetPath]
    "build" proj $ concat [["build", "--target", targetSpec], featureArgs, args]

cargoTest :: Partial => String -> String -> [String] -> Action ()
cargoTest buildDir proj args = do
  cargo buildDir [] "test" proj args

cargoMiriTest :: Partial => String -> String -> [String] -> Action ()
cargoMiriTest buildDir proj args = do
  cargo buildDir [] "clean" proj []
  command_ (cargoEnv buildDir) "cargo" $
    concat [["miri", "test", "-p", proj], args]
