module SimpleParser (readExpr) where

import Syntax
import Text.ParserCombinators.Parsec hiding (spaces)
import Prelude hiding (toRational)
import Numeric (readOct, readHex)
import Data.Char (toLower)
import Data.Complex (realPart, Complex (..))
import Data.Ratio

-- parse toma: Un Parser como primer parametro
--             Un String como nombre de la entrada, para los errores.
--             La entrada
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right value  -> value

parseExpr :: Parser LispVal
parseExpr = try parseNumber
            <|> try parseString
            <|> try  parseBool
            <|> try parseCharacter
            <|> try parseAtom
            <|> try parseQuoted
            <|> try parseListLike

-- Parseo de atomos
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do _ <- char '#'
               c <- oneOf "tf"
               (return . Bool) $ case c of
                                   't' -> True
                                   'f' -> False
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
                    (return . Character) $ case map toLower s of
                                             "spaces"  -> ' '
                                             ""        -> ' '
                                             "newline" -> '\n'
                                             [x]       -> x

-- Parseo de tipos numericos
parseNumber :: Parser LispVal
parseNumber = try parseComplexNumber
           <|> try parseFloat
           <|> try parseRationalNumber
           <|> try parseInteger

parseInteger :: Parser LispVal
parseInteger = try parseIntWithPrefix <|> try parsePlainInt

parseIntWithPrefix :: Parser LispVal
parseIntWithPrefix = do _ <- char '#'
                        try parseBin <|> try parseDec <|> try parseOct <|> try parseHex

parsePlainInt :: Parser LispVal
parsePlainInt = (Number . read) <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do int <- many1 digit
                _       <- char '.'
                decimal <- many1 digit
                let number = int ++ "." ++ decimal
                return $ (Float . read) number

-- Toma un numero de lisp y devuelve un numero de haskell
-- asumo que ese numero puede es real (un double)
fromNumber :: LispVal -> Double
fromNumber (Number x)  = fromIntegral x
fromNumber (Float x)    = x
fromNumber (Rational x) = fromRational x
fromNumber (Complex x)  = realPart x

parseComplexNumber :: Parser LispVal
parseComplexNumber = do real <- fromNumber <$> (try parseFloat <|> try parseRationalNumber <|> try parseInteger)
                        _ <- oneOf "+-"
                        imag <- fromNumber <$> (try parseFloat <|> try parseRationalNumber <|> try parseInteger)
                        return $ Complex (real :+ imag)

parseRationalNumber :: Parser LispVal
parseRationalNumber = do p <- read <$> many1 digit
                         _ <- char '/'
                         q <- read <$> many1 digit
                         return $ Rational (p % q)

-- Parseo de numeros en diferentes bases numericas
readBin :: Char -> Int
readBin '0' = 0
readBin '1' = 1

fromBinary :: String -> Int
fromBinary s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map readBin (reverse s)

parseBin :: Parser LispVal
parseBin = do _ <- char 'b'
              number <- many1 (oneOf "01")
              return $ (Number . fromBinary) number

parseDec :: Parser LispVal
parseDec = do _ <- char 'd'
              number <- many1 digit
              return $ (Number . read) number

parseOct :: Parser LispVal
parseOct = do _ <- char 'o'
              number <- many1 (oneOf "01234567")
              return $ (Number . fst . head . readOct) number

parseHex :: Parser LispVal
parseHex = do _ <- char 'x'
              number <- many1 (oneOf "0123456789abcdefABCDEF")
              return $ (Number . fst . head . readHex) number


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
