module Build.Kernel where

import Data.Maybe
import System.Directory

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Build.Rust

buildKernel :: String -> String -> Rules ()
buildKernel kernelDir buildDir = do
  let kernelBuildDir = buildDir </> "kernel"
  let kernelSrcDir = kernelDir </> "src"
  let kernelLinkScriptPath = kernelDir </> "link"

  let kernelLib = kernelBuildDir </> "libsyzygy_kernel.a"

  kernelBuildDir </> "sz_kernel.elf" %> \out -> do
    arch <- getConfig "ARCH"
    let linkScript = kernelLinkScriptPath </> (fromJust arch) <.> "ld"
    need [kernelLib, linkScript]

    cmd_ "ld -static -nostdlib --as-needed --gc-sections -z max-page-size=0x1000"
      "-T" linkScript "-o" [out] [kernelLib]

  kernelLib %> \out -> do
    need [kernelDir </> "Cargo.toml", kernelDir </> "build.rs"]
    target <- getConfig "TARGET"
    rustFeatures <- getConfig "KERNEL_FEATURES"
    
    rustSrc <- getDirectoryFiles kernelSrcDir ["//*.rs"]
    let rustSrcPath = [kernelSrcDir </> f | f <- rustSrc]
    need rustSrcPath

    let targetSpec = fromJust target
    cargoBuild kernelBuildDir targetSpec "syzygy_kernel" (words $ fromJust $ rustFeatures)
      ["-Zbuild-std=core,alloc,compiler_builtins", "-Zpackage-features"]
    liftIO $ copyFile (kernelBuildDir </> targetSpec </> "debug" </> "libsyzygy_kernel.a") out

  phony "kernelTest" $ do
    need [kernelLib]
    cargoTest kernelBuildDir "syzygy_kernel" []

  -- Warning: Very slow!
  phony "kernelMiriTest" $ do
    need [kernelLib]
    -- spin::exclusion is way too slow for Miri threads.
    -- bitmap uses inline assembly, which Miri does not support.
    cargoMiriTest kernelBuildDir "syzygy_kernel" ["--", "--skip", "exclusion", "--skip", "bitmap"]

  phony "kernelDoc" $ do
    need [kernelLib]
    cargoDoc kernelBuildDir "syzygy_kernel" []

  phony "kernelClippy" $ do
    need [kernelLib]

    target <- getConfig "TARGET"
    cargoClippy kernelBuildDir (fromJust target) "syzygy_kernel"
      ["-Zbuild-std=core,alloc,compiler_builtins"]
