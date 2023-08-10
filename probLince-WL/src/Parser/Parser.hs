module Parser.Parser where

import Parser.TestParser
import Parser.DTermParser
import Parser.TermParser
import Parser.DiffSystemParser
import Text.Parsec
import qualified Text.Parsec.Token as L
import Parser.LanguageDef

data Instruction = SKIP 
                 | Decl String Term 
                 | Asg String Term
                 | Seq Instruction Instruction 
                 | Cond Test (Instruction,Instruction) 
                 | Wh Test Instruction
                 | Wait Term Instruction 
                 | Diff DiffSystem Term deriving Show

-- Diff ([Double] -> [Double])
parseProg :: Parsec String st Instruction
parseProg = parseSeq <* L.whiteSpace hybridLangLexer <* eof

-- Parses the sequence operator
parseSeq :: Parsec String st Instruction
parseSeq = (chainl1 (L.whiteSpace hybridLangLexer *> parseInstruction) op) 
    where
        op = char ';' *> return Seq

parseInstruction :: Parsec String st Instruction
parseInstruction = try parseSkip 
                <|> try parseDecl 
                <|> try parseAsg 
                <|> try parseCond 
                <|> try parseWh 
                <|> try parseWait 
                <|> parseDiff

-- Parses the skip instruction
parseSkip :: Parsec String st Instruction
parseSkip = string "skip" *> return SKIP 

-- Parses a variable declaration
-- Example of a declaration: var X := 2.0
parseDecl :: Parsec String st Instruction
parseDecl = Decl <$> declVar <*> declValue
    where
        declVar = string "var" *> space *> spaces *> L.identifier hybridLangLexer <* spaces <* string ":="
        declValue = spaces *> parseTerm 

-- Parses an attribution of a value to a variable
-- Example of an attribution: X:=2.0
parseAsg :: Parsec String st Instruction
parseAsg = Asg <$> atribVar <*> atribValue
    where
        atribVar = parseVar <* spaces <* string ":=" >>= (\(Var x) -> return x)
        atribValue = spaces *> parseTerm

-- Parses the if statement
-- Example: if {...} then {...} else {...}
parseCond :: Parsec String st Instruction
parseCond = Cond <$> condition <*> ((,) <$> caseThen <*> caseElse) 
    where
        condition = string "if" *> space *> spaces *> parseTests <* (try endOfLine <|> space)
        caseThen = L.whiteSpace hybridLangLexer *> string "then" *> spaces *> ((between (char '{') (L.whiteSpace hybridLangLexer *> char '}') parseSeq) <|> parseInstruction)
        caseElse = L.whiteSpace hybridLangLexer *> string "else" *> spaces *> ((between (char '{') (L.whiteSpace hybridLangLexer *> char '}') parseSeq) <|> parseInstruction)

-- Parses the while statement
-- Example: while true do ... or while true do {...}
parseWh :: Parsec String st Instruction
parseWh = Wh <$> whHeader <*> whBody
    where
        whHeader = string "while" *> space *> spaces *> parseTests <* space <* spaces <* string "do" <* (try endOfLine <|> space)
        whBody = try (L.whiteSpace hybridLangLexer *> parseInstruction)
              <|> (L.whiteSpace hybridLangLexer *> between (char '{') (L.whiteSpace hybridLangLexer *> char '}') parseSeq)

-- Parses the wait call
-- Example: wait 10 do ... or wait 20 do {...}
parseWait :: Parsec String st Instruction
parseWait = Wait <$> waitHeader <*> waitBody 
    where
        waitHeader = string "wait" *> space *> parseTerm <* space <* string "do" <* (try endOfLine <|> space)
        waitBody = try (L.whiteSpace hybridLangLexer *> parseInstruction)
                <|> (L.whiteSpace hybridLangLexer *> between (char '{') (L.whiteSpace hybridLangLexer *> char '}') parseSeq)

parseDiff :: Parsec String st Instruction
parseDiff = Diff <$> parseDiffSystem <*> (string "for" *> space *> spaces *> parseTerm)
