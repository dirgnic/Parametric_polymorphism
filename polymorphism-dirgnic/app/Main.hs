-- src/Main.hs
module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile)

import Parsing (parseProgram)
import Monomorphize (monomorphizeProgram)
import Lowering (lowerProgram)
import IRInterpreter (runLoweredProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> compileAndRun file
    _ -> die "Usage: try2 <file>"

compileAndRun :: FilePath -> IO ()
compileAndRun path = do
  src <- readFile path
  case parseProgram src of
    Left err -> die (show err)
    Right ast -> do
      let mono = monomorphizeProgram ast
      putStrLn "=== After Monomorphization ==="
      mapM_ print mono
      let ir = lowerProgram mono
      case runLoweredProgram ir of
        Left err -> die ("Runtime error: " ++ err)
        Right val -> print val
