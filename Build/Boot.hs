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
  needed . map (prefix </>) . concatMap snd . parseMakefile =<< liftIO (readFile makefile)

buildBootloader :: String -> Rules ()
buildBootloader limineDir = do
  -- Use gcc as a proxy dependency for the whole toolchain.
  let limineCC = limineDir </> "toolchain" </> "bin" </> "i386-elf-gcc"
  options <- getShakeOptionsRules
  let threads = shakeThreads options

  limineDir </> "limine.bin" %> \_out -> do
    need [
      limineCC,
      limineDir </> "Makefile",
      limineDir </> "decompressor" </> "Makefile",
      limineDir </> "stage2" </> "Makefile"
      ]

    cmd_ "make -j" (show threads) "-C" limineDir "bootloader"
    decompressorDepFiles <- getDirectoryFiles limineDir ["decompressor//*.d"]
    stage2DepFiles <- getDirectoryFiles limineDir ["stage2//*.d"]
    forM_ (map (limineDir </>) decompressorDepFiles) (prefixedMakefileDeps $ limineDir </> "decompressor")
    forM_ (map (limineDir </>)  stage2DepFiles) (prefixedMakefileDeps $ limineDir </> "stage2")

  limineDir </> "limine.o" %> \_out -> do
    need [limineDir </> "limine.bin"]
    cmd_ "make -C" limineDir "limine.o"

  limineDir </> "limine-install" %> \_out -> do
    need [limineDir </> "Makefile"]
    makefile <- liftIO (readFile $ limineDir </> "Makefile")
    let makefileDeps = map (limineDir </>) $
                       Map.fromListWith (++) (parseMakefile makefile) Map.! "limine-install"
    need makefileDeps

    cmd_ "make -j" (show threads) "-C" limineDir "limine-install"

  limineCC %> \_out -> do
    need [limineDir </> "toolchain" </> "make_toolchain.sh"]
    cmd_ (Cwd $ limineDir </> "toolchain") "./make_toolchain.sh" ("-j" ++ show threads)
