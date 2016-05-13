module SimpleParser (parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- parser que reconoce uno de los simbolos permitidos
-- en los identificadores de Scheme
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parser que reconoce espacios
spaces :: Parser ()
spaces = skipMany1 space

-- parse toma: Un Parser como primer parametro
--             Un String como nombre de la entrada, para los errores.
--             La entrada
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

parseExpr :: IO ()
parseExpr = do (expr:_) <- getArgs
               putStrLn (readExpr expr)


