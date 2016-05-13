module SimpleParser (parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- parser que reconoce uno de los simbolos permitidos
-- en los identificadores de Scheme
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parse toma: Un Parser como primer parametro
--             Un String como nombre de la entrada, para los errores.
--             La entrada
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

parseExpr :: IO ()
parseExpr = do (expr:_) <- getArgs
               putStrLn (readExpr expr)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             
