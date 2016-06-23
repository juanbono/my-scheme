module Syntax where
import           Data.Complex (Complex (..))

instance Show LispVal where
  show (Atom s)          = s
  show (List xs)         = "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Number x)        = show x
  show (String s)        = "\"" ++ s ++ "\""
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (Character c)     = [c]
  show (Float x)         = show x
  show (Complex x)       = show x
  show (Rational x)      = show x

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Int
             | Bool Bool
             | Character Char
             | String String
             | Float Double
             | Complex (Complex Double)
             | Rational Rational

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
