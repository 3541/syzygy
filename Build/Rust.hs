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

cargo :: Partial => String -> String -> String -> [String] -> [String] -> Action ()
cargo buildDir targetSpec targetProj features args = do
  currentDir <- liftIO getCurrentDirectory
  let targetPath = currentDir </> "targets"
  let featureArgs = if length features > 0
        then ["--features", intercalate "," features]
        else []
  command_ (concat [cargoEnv buildDir, [AddEnv "RUST_TARGET_PATH" targetPath]])
    "cargo" $ concat [["build", "--target", targetSpec, "-p", targetProj], featureArgs, args]
