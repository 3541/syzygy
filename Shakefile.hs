module Main where

import Build.Boot
import Build.Kernel
import Build.Rust
import Control.Exception.Extra
import Data.Map.Strict as Map
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

buildDir = "_build"

imageBuildDir = buildDir </> "image"

imageFiles :: Map.Map String String
imageFiles =
  Map.fromList
    [ (imageBuildDir </> "limine.cfg", "boot" </> "limine.cfg"),
      (imageBuildDir </> "sz_kernel.elf", buildDir </> "kernel" </> "sz_kernel.elf"),
      (imageBuildDir </> "limine.sys", "boot" </> "limine" </> "bin" </> "limine.sys")
    ]

scriptPath = AddPath [] ["./scripts"]

qemuRun :: Partial => [String] -> FilePath -> Action ()
qemuRun options image = do
  need [image]

  arch <- getConfig "ARCH"
  let qemuName = "qemu-system-" ++ fromJust arch
  qemuMemory <- getConfig "QEMU_MEMORY"
  qemuKvmConfig <- getConfig "QEMU_KVM"
  let qemuKvm =
        if fromJust qemuKvmConfig == "yes"
          then "-enable-kvm"
          else ""
  qemuDisplayConfig <- getConfig "QEMU_DISPLAY"
  let qemuDisplay =
        if fromJust qemuDisplayConfig == "yes"
          then ""
          else "-display none"
  qemuMonitorConfig <- getConfig "QEMU_MONITOR"
  let qemuMonitor =
        if fromJust qemuMonitorConfig == "yes"
          then "-monitor telnet:127.0.0.1:7777,server,nowait"
          else ""

  cmd_
    qemuName
    qemuKvm
    qemuDisplay
    qemuMonitor
    "-m"
    qemuMemory
    "-d cpu_reset -s -debugcon stdio"
    ("-drive format=raw,file=" ++ image)
    options

main :: IO ()
main = shakeArgs
  shakeOptions {shakeProgress = progressSimple, shakeColor = True, shakeThreads = 0}
  $ do
    usingConfigFile $ "cfg" </> "shake.cfg"
    want [buildDir </> "syzygy.img"]

    phony "clean" $ do
      putInfo "Cleaning..."
      removeFilesAfter buildDir ["//*"]
      cleanBoot $ "boot" </> "limine"

    phony "cleanMeta" $ do
      putInfo "Cleaning Shake metadata..."
      removeFilesAfter ".shake" ["//*"]

    phony "build" $ do
      -- Useful for making sure changes build without the overhead of making an image.
      need [buildDir </> "kernel" </> "sz_kernel.elf"]

    phony "test" $ do
      need ["kernelTest"]

    phony "testHeavy" $ do
      need ["test", "kernelMiriTest"]

    phony "lint" $ do
      need ["kernelClippy"]

    phony "run" $ do
      -- TODO: Intelligently handle not having qemu (run Bochs instead)
      qemuRun [] $ buildDir </> "syzygy.img"

    phony "debug" $ do
      qemuRun ["-S"] $ buildDir </> "syzygy.img"

    buildDir </> "syzygy.img" %> \out -> do
      need
        ( Map.keys imageFiles
            ++ [ "boot" </> "limine" </> "bin" </> "limine-install",
                 "boot" </> "limine" </> "bin" </> "limine-hdd.bin",
                 "scripts" </> "make_fs.sh"
               ]
        )
      cmd_
        NoProcessGroup
        InheritStdin
        scriptPath
        Shell
        ("sudo env \"PATH=$PATH\" make_fs.sh " ++ "boot" </> "limine")
        [imageBuildDir]
        [out]

    keys imageFiles |%> \out -> do
      copyFileChanged (imageFiles ! out) out

    rustVersion $ buildDir </> "kernel"
    buildKernel "kernel" buildDir
    buildBootloader $ "boot" </> "limine"
