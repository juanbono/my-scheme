module Main where

import qualified SimpleParser as Parser
import System.Environment

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn (Parser.readExpr expr)
