module Main where

import qualified SimpleParser as Parser
import Eval
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . Parser.readExpr . head
