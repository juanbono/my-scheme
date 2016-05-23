{-# LANGUAGE GADTs #-}
module Syntax where
import Data.Complex (Complex (..))

instance Show LispVal where
  show (Atom s)          = s
  show (List xs)         = "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Number x)        = show x
  show (String s)        = "\"" ++ s ++ "\""
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (Character c)     = [c]

data LispVal where
  Atom       :: String -> LispVal
  List       :: [LispVal] -> LispVal
  DottedList :: [LispVal] -> LispVal -> LispVal
  Number     :: NumberType a -> LispVal
  String     :: String -> LispVal
  Bool       :: Bool -> LispVal
  Character  :: Char -> LispVal

data NumberType a where
  Float    :: Double -> NumberType Double
  Complex  :: Complex Double -> NumberType (Complex Double)
  Rational :: Rational -> NumberType Rational
  Integer  :: Int -> NumberType Int

instance Show (NumberType a) where
  show (Float x)    = show x
  show (Complex x)  = show x
  show (Rational x) = show x
  show (Integer x)  = show x

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

