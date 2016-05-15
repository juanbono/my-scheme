module SimpleParser (main) where

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
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _  -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseEscapedChar :: Parser Char
parseEscapedChar = do _ <- char '\\'
                      c <- oneOf ['\\', '"', 'n', 'r', 't']
                      return $ case c of
                                 '\\' -> c
                                 '"'  -> c
                                 'n'  -> '\n'
                                 'r'  -> '\r'
                                 't'  -> '\t'

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (parseEscapedChar <|> noneOf ['\\', '"'])
                 _ <- char '"'
                 return (String x)

-- <|> es un operador de eleccion que intenta parsear con el primer parser,
-- si falla entonces intenta parsear con el segundo.
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
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
parseNumber = (Number . read) <$> many1 digit

-- parsea una serie de expresiones separadas por espacios y luego aplica el constructor List.
parseList :: Parser LispVal
parseList =  List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do hd <- endBy parseExpr spaces
                     tl <- char '.' >> spaces >> parseExpr
                     return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = do _ <- char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseListLike :: Parser LispVal
parseListLike = do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseListLike

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn (readExpr expr)

{-
Exercises: Writing a Simple Parser
---------
1. Rewrite parseNumber, without liftM, using
     * do-notation
     * explicit sequencing with the >>= operator

2. Our strings aren't quite R5RS compliant, because they don't support escaping of internal quotes within the string.
   Change parseString so that \" gives a literal quote character instead of terminating the string. You may want to
   replace noneOf "\"" with a new parser action that accepts either a non-quote character or a backslash followed by a quote mark.

3. Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters.

4. Change parseNumber to support the Scheme standard for different bases. You may find the readOct and readHex functions useful.
   TODO
5. Add a Character constructor to LispVal, and create a parser for character literals as described in R5RS.
   Add a Float constructor to LispVal, and support R5RS syntax for decimals. The Haskell function readFloat may be useful.
   TODO
6. Add data types and parsers to support the full numeric tower of Scheme numeric types. Haskell has built-in types to
   represent many of these; check the Prelude. For the others, you can define compound types that represent
   eg. a Rational as a numerator and denominator, or a Complex as a real and imaginary part (each itself a Real).
   TODO
Exercises: Recursive Parsers
----------------------------
1. Add support for the backquote syntactic sugar: The Scheme standard details what it should expand into (quasiquote/unquote).
   TODO
2. Add support for vectors. The Haskell representation is up to you: GHC does have an Array data type, but it can be difficult
   to use. Strictly speaking, a vector should have a constant-time indexing and updating, but destructive update in a purely
   functional language is difficult. You may have a better idea how to do this after the section on set!, later in this tutorial.
   TODO
3. Instead of using the try combinator, left-factor the grammar so that the common subsequence is its own parser. You should end
   up with a parser that matches a string of expressions, and one that matches either nothing or a dot and a single expression.
   Combining the return values of these into either a List or a DottedList is left as a (somewhat tricky) exercise for the reader:
   you may want to break it out into another helper function.
   TODO
-}
