module Build.Kernel where

import Data.Maybe

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Build.Rust

buildKernel :: String -> String -> Rules ()
buildKernel kernelDir buildDir = do
  let kernelBuildDir = buildDir </> "kernel"
  let kernelSrcDir = kernelDir </> "src"
  let kernelLinkScriptPath = kernelDir </> "link"

  kernelBuildDir </> "sz_kernel.elf" %> \out -> do
    arch <- getConfig "ARCH"
    asmSrc <- getDirectoryFiles kernelSrcDir ["//" ++ (fromJust arch) ++ "//*.asm"]
    let asmObj = [kernelBuildDir </> f -<.> ".asm.o" | f <- asmSrc]
    let linkScript = kernelLinkScriptPath </> (fromJust arch) <.> "ld"
    need (concat [asmObj, [kernelBuildDir </> "libsyzygy_kernel.a", linkScript]])

    cmd_ "ld -nostdlib -n --gc-sections -T" linkScript
      "-o" [out] asmObj [kernelBuildDir </> "libsyzygy_kernel.a"]

  kernelBuildDir ++ "//*.asm.o" %> \out -> do
    let src = kernelSrcDir </> (dropDirectory1 $ dropDirectory1 $ (dropExtension out) -<.> "asm")
    need [src]
    cmd_ "nasm -felf64" [src] "-o" [out]

  kernelBuildDir </> "libsyzygy_kernel.a" %> \out -> do
    arch <- getConfig "ARCH"
    rustFeatures <- getConfig "KERNEL_FEATURES"
    
    rustSrc <- getDirectoryFiles kernelSrcDir ["//*.rs"]
    let rustSrcPath = [kernelSrcDir </> f | f <- rustSrc]
    need rustSrcPath

    let targetSpec = fromJust arch ++ "-pc-elf-none"
    cargo kernelBuildDir targetSpec "syzygy_kernel" (words $ fromJust $ rustFeatures)
      ["-Zbuild-std=core,alloc,compiler_builtins", "-Zpackage-features"]
    copyFileChanged (kernelBuildDir </> targetSpec </> "debug" </> "libsyzygy_kernel.a") out
