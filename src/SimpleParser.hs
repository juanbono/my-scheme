module SimpleParser (parseExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)

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
readExpr input = case parse parseLispExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _ -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return (String x)
-- <|> es un operador de eleccion que intenta parsear con el primer parser,
-- si falla entonces intenta parsear con el segundo.
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

-- many1 matchea 1 o mas
-- many1 digit matchea con 1 o mas digitos.
-- Luego al string parseado se le aplica read (se lo pasa a Integer)
-- y luego se construye un Number a partir de ese valor. Todo dentro de
-- el contexto monadico de Parser (liftM pasa (Number . read) al contexto)
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
              -- (Number . read) <$> many1 digit tambien puede ser

parseLispExpr :: Parser LispVal
parseLispExpr = parseAtom <|> parseString <|> parseNumber

parseExpr :: IO ()
parseExpr = do (expr:_) <- getArgs
               putStrLn (readExpr expr)
