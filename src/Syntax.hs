module Syntax
  ( LispVal (..)
  , unwordsList
  , ThrowsError
  , trapError
  , extractValue,
  LispError (..)
  ) where
import           Data.Complex (Complex (..))
import Text.ParserCombinators.Parsec
import Control.Monad.Except

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
             | PrimitiveFunc [LispVal -> ThrowsError LispVal]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- Error
type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (Default message) = message

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
