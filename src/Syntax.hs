module Syntax where

import Data.Complex (Complex (..))

instance Show LispVal where
  show (Atom s) =  s
  show (List xs) =  "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Number x) = show x
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = [c]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number NumberType
             | String String
             | Bool Bool
             | Character Char

data NumberType = Integer Int
                | Float Double
                | Complex (Complex Double)
                | Rational Rational
  deriving Show -- hacer la instancia a mano

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

toFloat :: Double -> LispVal
toFloat = Number . Float

toRational :: Rational -> LispVal
toRational = Number . Rational

toComplex :: Complex Double -> LispVal
toComplex = Number . Complex

toInt :: Int -> LispVal
toInt = Number . Integer

