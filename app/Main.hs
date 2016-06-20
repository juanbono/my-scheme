module Main where

import qualified SimpleParser as Parser
import Eval
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ Parser.readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
