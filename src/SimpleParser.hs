module SimpleParser (readExpr) where

import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)
import Prelude hiding (toRational)
import Numeric (readOct, readHex)
import Data.Char (toLower)
import Data.Complex (Complex (..))
import Data.Ratio

-- parse toma: Un Parser como primer parametro
--             Un String como nombre de la entrada, para los errores.
--             La entrada
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _  -> "Found value"

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseCharacter
         <|> parseNumber
         <|> parseQuoted
         <|> parseListLike

-- Parseo de atomos
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

-- Parseo de strings
parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (parseEscapedChar <|> noneOf ['\\', '"'])
                 _ <- char '"'
                 return (String x)

parseEscapedChar :: Parser Char
parseEscapedChar = do _ <- char '\\'
                      c <- oneOf ['\\', '"', 'n', 'r', 't']
                      return $ case c of
                                 '\\' -> c
                                 '"'  -> c
                                 'n'  -> '\n'
                                 'r'  -> '\r'
                                 't'  -> '\t'

-- Parseo de caracteres
parseCharacter :: Parser LispVal
parseCharacter = do _ <- string "#\\"
                    s <- many letter
                    (return . Character) $ case (map toLower s) of
                                             "spaces"  -> ' '
                                             ""        -> ' '
                                             "newline" -> '\n'
                                             [x]       -> x

-- Parseo de tipos numericos
parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseNumberWithPrefix -- corregir

parseNumberWithPrefix :: Parser LispVal
parseNumberWithPrefix = do _ <- char '#'
                           parseBin <|> parseDec <|> parseOct <|> parseHex

parsePlainNumber :: Parser LispVal
parsePlainNumber = (Number . Integer. read) <$> many1 digit -- corregir?

parseFloat :: Parser LispVal
parseFloat = do int <- many1 digit
                _       <- char '.'
                decimal <- many1 digit
                let number = int ++ "." ++ decimal
                return $ (toFloat . read) number

-- Toma un numero de lisp y devuelve un numero de haskell
-- asumo que ese numero puede es real (un double)
fromNumber :: Num a => LispVal -> a
fromNumber (Number (Integer x))  = undefined
fromNumber (Number (Float x))    = undefined
fromNumber (Number (Complex x))  = undefined
fromNumber (Number (Rational x)) = undefined

parseComplexNumber :: Parser LispVal
parseComplexNumber = do real <- fromNumber <$> (try parsePlainNumber <|> parseFloat <|> parseRationalNumber)
                        _ <- oneOf "+-"
                        imaginary <- fromNumber <$> (try parsePlainNumber <|> parseFloat <|> parseRationalNumber)
                        return $ toComplex (real :+ imaginary)

parseRationalNumber :: Parser LispVal
parseRationalNumber = do p <- read <$> many1 digit
                         _ <- char '/'
                         q <- read <$> many1 digit
                         return $ toRational (p % q)

-- Parseo de numeros en diferentes bases numericas
readBin :: String -> Int
readBin = undefined  -- TODO: hacer funcion para pasar numeros binarios a enteros

parseBin :: Parser LispVal
parseBin = do _ <- char 'b'
              number <- many1 (oneOf "01")
              return $ (toInt . readBin) number

parseDec :: Parser LispVal
parseDec = do _ <- char 'd'
              number <- many1 digit
              return $ (toInt . read) number

parseOct :: Parser LispVal
parseOct = do _ <- char 'o'
              number <- many1 (oneOf "01234567")
              return $ (toInt . fst . head . readOct) number

parseHex :: Parser LispVal
parseHex = do _ <- char 'x'
              number <- many1 (oneOf "0123456789abcdefABCDEF")
              return $ (toInt . fst . head . readHex) number


-- Parseo de listas y listas puntuadas
parseListLike :: Parser LispVal
parseListLike = do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

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

-- Funciones Auxiliares
-- parser que reconoce uno de los simbolos permitidos
-- en los identificadores de Scheme
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parser que reconoce espacios
spaces :: Parser ()
spaces = skipMany1 space
