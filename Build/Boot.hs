module Build.Boot where

import Control.Exception.Extra
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

-- Clean the Limine build directory.
cleanBoot :: Partial => FilePath -> Action ()
cleanBoot limineDir = do
  cmd_ "make -C" limineDir "clean"
  removeFilesAfter limineDir ["limine.bin", "bin" </> "limine-install"]

prefixedMakefileDeps :: FilePath -> FilePath -> Action ()
prefixedMakefileDeps prefix makefile =
  needed . map (prefix </>) . concatMap snd . parseMakefile =<< liftIO (readFile makefile)

buildBootloader :: String -> Rules ()
buildBootloader limineDir = do
  -- Use gcc as a proxy dependency for the whole toolchain.
  let limineCC = limineDir </> "toolchain" </> "bin" </> "x86_64-elf-gcc"
  options <- getShakeOptionsRules
  let threads = shakeThreads options
  let limineHddBin = limineDir </> "bin" </> "limine-hdd.bin"

  limineHddBin %> \_out -> do
    need
      [ limineCC,
        limineDir </> "Makefile",
        limineDir </> "decompressor" </> "Makefile",
        limineDir </> "stage23" </> "Makefile",
        limineDir </> "stivale" </> "stivale2.h"
      ]

    cmd_ "make -j" (show threads) "-C" limineDir "limine-bios"
    decompressorDepFiles <- getDirectoryFiles limineDir ["decompressor//*.d"]
    stage2DepFiles <- getDirectoryFiles limineDir ["stage23//*.d"]
    forM_
      (map (limineDir </>) decompressorDepFiles)
      (prefixedMakefileDeps $ limineDir </> "decompressor")
    forM_ (map (limineDir </>) stage2DepFiles) (prefixedMakefileDeps $ limineDir </> "stage23")

  limineDir </> "bin" </> "limine.sys" %> \_out -> do
    need [limineHddBin]

  limineDir </> "stivale" </> "stivale2.h" %> \_out -> do
    need [limineDir </> "Makefile"]
    cmd_ "rmdir" "--ignore-fail-on-non-empty" (limineDir </> "stivale")
    cmd_ "make" "-C" limineDir "stivale"

  limineDir </> "limine-install" </> "limine-hdd.o" %> \_out -> do
    need [limineHddBin]
    cmd_
      "make"
      "-C"
      (limineDir </> "limine-install")
      ("LIMINE_HDD_BIN=" ++ (".." </> "bin" </> "limine-hdd.bin"))
      "limine-hdd.o"

  limineDir </> "bin" </> "limine-install" %> \_out -> do
    need [limineDir </> "limine-install" </> "Makefile", limineHddBin]
    makefile <- liftIO (readFile $ limineDir </> "limine-install" </> "Makefile")
    let makefileDeps =
          filter (notElem '$') $
            map ((limineDir </> "limine-install") </>) $
              Map.fromListWith (++) (parseMakefile makefile) Map.! "limine-install"
    need makefileDeps

    cmd_ "make -j" (show threads) "-C" limineDir ("bin" </> "limine-install")

  limineCC %> \_out -> do
    need [limineDir </> "scripts" </> "make_toolchain.sh"]
    cmd_ (Cwd limineDir) ("scripts" </> "make_toolchain.sh") "toolchain" ("-j" ++ show threads)
