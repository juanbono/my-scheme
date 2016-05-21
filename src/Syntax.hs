{-# LANGUAGE GADTs #-}
module Syntax where
import Data.Complex (Complex (..))

instance Show (LispVal a) where
  show (Atom s) =  s
  show (List xs) =  "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Number x) = show x
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = [c]

data LispVal a where
  Atom       :: String -> LispVal String
  List       :: [LispVal a] -> LispVal [a]
  DottedList :: [LispVal a] -> LispVal a -> LispVal ([LispVal a], LispVal a)
  Number     :: NumberType a -> LispVal a
  String     :: String -> LispVal String
  Bool       :: Bool -> LispVal Bool
  Character  :: Char -> LispVal Char

data NumberType a where
  Float    :: Double -> NumberType Double
  Complex  :: Complex Double -> NumberType (Complex Double)
  Rational :: Rational -> NumberType Rational
  Integer  :: Int -> NumberType Int

instance Show (NumberType a) where
  show (Float x) = show x
  show (Complex x) = show x
  show (Rational x) = show x
  show (Integer x) = show x

unwordsList :: [LispVal a] -> String
unwordsList = unwords . map show

toFloat :: Double -> LispVal Double
toFloat = Number . Float

toRational :: Rational -> LispVal Rational
toRational = Number . Rational

toComplex :: Complex Double -> LispVal (Complex Double)
toComplex = Number . Complex

toInt :: Int -> LispVal Int
toInt = Number . Integer

