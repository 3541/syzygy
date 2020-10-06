module Build.Boot where

import Control.Exception.Extra
import Data.Foldable
import qualified Data.Map.Strict as Map

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

-- Clean the Limine build directory.
cleanBoot :: Partial => FilePath -> Action ()
cleanBoot limineDir = do
  cmd_ "make -C" limineDir "clean"
  removeFilesAfter limineDir ["limine.bin", "limine-install"]

prefixedMakefileDeps :: FilePath -> FilePath -> Action ()
prefixedMakefileDeps prefix makefile =
  needed . map (\f -> prefix </> f) . concatMap snd . parseMakefile =<< liftIO (readFile makefile)

buildBootloader :: String -> Rules ()
buildBootloader limineDir = do
  let limineCC = limineDir </> "toolchain" </> "bin" </> "i386-elf-gcc"
  options <- getShakeOptionsRules
  let threads = shakeThreads options

  limineDir </> "limine.bin" %> \out -> do
    need [
      limineCC,
      limineDir </> "Makefile",
      limineDir </> "decompressor" </> "Makefile",
      limineDir </> "stage2" </> "Makefile"
      ]

    cmd_ "make -j" (show threads) "-C" limineDir "all"
    decompressorDepFiles <- getDirectoryFiles limineDir ["decompressor//*.d"]
    stage2DepFiles <- getDirectoryFiles limineDir ["stage2//*.d"]
    forM_ (map (\f -> limineDir </> f) decompressorDepFiles) (prefixedMakefileDeps $ limineDir </> "decompressor")
    forM_ (map (\f -> limineDir </> f) stage2DepFiles) (prefixedMakefileDeps $ limineDir </> "stage2")

  limineDir </> "limine-install" %> \out -> do
    need [limineDir </> "Makefile"]
    makefile <- liftIO (readFile $ limineDir </> "Makefile")
    let makefileDeps = parseMakefile makefile
    need $ (Map.fromList makefileDeps) Map.! "limine-install"

    cmd_ "make -j" (show threads) "-C" limineDir "limine-install"

  limineCC %> \out -> do
    need [limineDir </> "toolchain" </> "make_toolchain.sh"]
    cmd_ (Cwd $ limineDir </> "toolchain") "./make_toolchain.sh" ("-j" ++ (show threads))
