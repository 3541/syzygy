module Main where

import Data.Map.Strict as Map
import Data.Maybe
import System.Process

import Development.Shake
import Development.Shake.Command
import Development.Shake.Config
import Development.Shake.FilePath

import Build.Kernel

buildDir = "_build"
imageBuildDir = buildDir </> "image"

imageFiles :: Map.Map String String
imageFiles = Map.fromList [
  (imageBuildDir </> "boot" </> "grub" </> "grub.cfg", "boot" </> "grub.cfg"),
  (imageBuildDir </> "boot" </> "sz_kernel.elf", buildDir </> "kernel" </> "sz_kernel.elf")
  ]

scriptPath = AddPath [] ["./scripts"]


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = buildDir, shakeProgress = progressSimple, shakeColor = True} $ do
  usingConfigFile $ "cfg" </> "shake.cfg"
  want [buildDir </> "syzygy.img"]

  phony "clean" $ do
    putInfo "Cleaning..."
    removeFilesAfter buildDir ["//*"]

  phony "build" $ do
    -- Useful for making sure changes build without the overhead of making an image.
    need $ elems imageFiles

  phony "run" $ do
    need [buildDir </> "syzygy.img"]

    arch <- getConfig "ARCH"
    let qemuName = "qemu-system-" ++ (fromJust arch)
    qemuMemory <- getConfig "QEMU_MEMORY"
    qemuKvmConfig <- getConfig "QEMU_KVM"
    let qemuKvm = if (fromJust qemuKvmConfig) == "yes"
          then "-enable-kvm"
          else ""
    qemuDisplayConfig <- getConfig "QEMU_DISPLAY"
    let qemuDisplay = if (fromJust qemuDisplayConfig) == "yes"
          then ""
          else "-display none"

    cmd_ qemuName qemuKvm qemuDisplay "-m" qemuMemory "-d cpu_reset -s -debugcon stdio"
      ("-drive format=raw,file=" ++ (buildDir </> "syzygy.img"))

  buildDir </> "syzygy.img" %> \out -> do
    need $ Map.keys imageFiles
    cmd_ NoProcessGroup InheritStdin scriptPath Shell
      "sudo env \"PATH=$PATH\" make_fs.sh" [imageBuildDir] [out]

  keys imageFiles |%> \out -> do
    copyFileChanged (imageFiles ! out) out

  buildKernel "kernel" buildDir
