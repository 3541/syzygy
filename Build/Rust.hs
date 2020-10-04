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

cargo :: Partial => String -> String -> String -> [String] -> [String] -> Action ()
cargo buildDir targetSpec targetProj features args = do
  need [buildDir </> "rustc.version"]
  currentDir <- liftIO getCurrentDirectory
  let targetPath = currentDir </> "targets"
  let featureArgs = if length features > 0
        then ["--features", intercalate "," features]
        else []
  command_ (concat [cargoEnv buildDir, [AddEnv "RUST_TARGET_PATH" targetPath]])
    "cargo" $ concat [["build", "--target", targetSpec, "-p", targetProj], featureArgs, args]
