module Parser.LanguageDef where

import Text.Parsec.Language
import Text.Parsec.Token
import qualified Text.Parsec.Token as L
import Text.Parsec

hybridLangDef :: LanguageDef st
hybridLangDef = emptyDef {
                commentStart = "/*",
                commentEnd = "*/",
                commentLine = "//",
                nestedComments = True,
                identStart = letter <|> oneOf "_",
                identLetter = alphaNum <|> oneOf "_",
                opStart = oneOf "+-/*<=>:!&|;",
                opLetter = oneOf "&|=",
                reservedNames = ["var", "if", "then", "else", "while", "wait", "true", "false", "coin()", "coin", "skip", "rand()", "rand", "normal", "_time_"],
                reservedOpNames = ["+", "-", "/", "*", "!", "||", "<", ">", "=", "&&",";"],
                caseSensitive = True
           }

hybridLangLexer :: TokenParser st
hybridLangLexer = L.makeTokenParser hybridLangDef
