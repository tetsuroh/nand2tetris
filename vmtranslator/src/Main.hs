module Main where

import System.Environment (getArgs)
import System.FilePath (takeDirectory, replaceFileName, takeBaseName, splitDirectories, (</>))
import System.Directory (doesFileExist, getDirectoryContents, setCurrentDirectory, getCurrentDirectory)

import Data.List (isSuffixOf)

import Language.Hack.VM.Compiler (compile)
import Language.Hack.VM.Parser (parseHackVM)

main :: IO ()
main = do
  [file] <- getArgs
  let directory = (++"/") . takeDirectory $ file
  isFile <- doesFileExist file
  files <- if isFile then
               return [file]
           else
               fmap (filter (".vm"`isSuffixOf`)) $
                    getDirectoryContents directory
  setCurrentDirectory directory
  currentDirectory <- getCurrentDirectory
  sources <- mapM readFile files
  print files
  let vmls = mapM (parseHackVM "Hack VM") sources
      baseNames = map takeBaseName files
      out = currentDirectory </> last (splitDirectories currentDirectory) ++ ".asm"
  case vmls of
    Right vs  -> do
             let asm = compile $ zip vs baseNames
             writeFile out asm
    Left errs -> print errs
