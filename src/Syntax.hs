module Syntax where

import Data.Complex (Complex (..))

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

toFloat :: Double -> LispVal
toFloat = Number . Float

toRational :: Rational -> LispVal
toRational = Number . Rational

toComplex :: Complex Double -> LispVal
toComplex = Number . Complex

toInt :: Int -> LispVal
toInt = Number . Integer

