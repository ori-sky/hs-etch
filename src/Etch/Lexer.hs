module Etch.Lexer where

import Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as Atto

identifierFirstChars :: [Char]
identifierFirstChars = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'
                       , 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V'
                       , 'W', 'X', 'Y', 'Z'
                       , 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'
                       , 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v'
                       , 'w', 'x', 'y', 'z'
                       , '_'
                       ]

identifierRestChars :: [Char]
identifierRestChars = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                      ] ++ identifierFirstChars

-- minus character must be at start of list
operatorChars :: [Char]
operatorChars = [ '-', '+', '*', '/', '<', '>', '=' ]

whitespaceParser :: Parser ()
whitespaceParser = skipSpace

charParser :: Char -> Parser ()
charParser c = char c *> whitespaceParser

charsParser :: [Char] -> Parser ()
charsParser cs = traverse char cs *> whitespaceParser

integerParser :: Parser Integer
integerParser = signed decimal <* whitespaceParser

identifierParser :: Parser ByteString
identifierParser = BS.cons
                <$> satisfy (inClass identifierFirstChars)
                <*> Atto.takeWhile (inClass identifierRestChars)
                <* whitespaceParser

operatorParser :: Parser ByteString
operatorParser = Atto.takeWhile1 (inClass operatorChars)
              <* whitespaceParser
