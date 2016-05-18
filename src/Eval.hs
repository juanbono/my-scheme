module Eval (eval) where

import Syntax
import SimpleParser

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*))]
             -- ("/", numericBinop div),
             -- ("mod", numericBinop mod),           -- Fijarme como hacer para que no defaultee a Integer
             -- ("quotient", numericBinop quot),     -- por el usod de div, mod, quot, rem
             -- ("remainder", numericBinop rem)]

numericBinop :: Num a => (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op params = undefined

{- Version dada por el libro
 - Pensar el foldl1 como un "reemplazador de funciones".
 - Reemplaza cada ocurrencia de ":" por la operacion "op".
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
-}
