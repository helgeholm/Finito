module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad

import Data

type SourceId = String

parseRoot :: SourceId -> [Char] -> Either ParseError Configuration
parseRoot inName stream =
  case (parse root inName stream) of
    Left err -> Left err
    Right ss -> Right $ Configuration inName ss

root = do ss <- many section
          eof
          return ss

section = do el
             char '['
             n <- name
             char ']'
             ws_eol
             asses <- assignments
             el
             return $ Section n asses

assignments = many $ try (el >> assignment)

assignment = try proper <|> notReally where
  proper    = do c <- cond
                 ws
                 char '='
                 ws
                 cl <- commaList
                 ws_eol
                 return $ ProperAssign c cl
  notReally = do c <- cond
                 ws_eol
                 return $ NotReallyAssign c

commaList = cond `sepBy` (char ',' >> ws)

name = do x  <- letter
          xs <- many (letter <|> digit <|> char '_')
          return $ Name (x : xs)

atom = quotedString <|> (liftM NameAtom name)

quotedString = do char '"'
                  str <- many (noneOf "\"")
                  char '"'
                  return $ StringAtom str
     
cond = try restricted <|> simple where
  restricted = do a <- atom
                  ws
                  char '@'
                  ws
                  c <- orRestrictor
                  return $ RestrCond a c
  simple     = SimpleCond `liftM` atom

orRestrictor = try proper <|> simple where
  proper = do a <- andRestrictor
              ws
              char '|'
              ws
              o <- orRestrictor
              return $ ProperOrRestrictor a o
  simple = SimpleOrRestrictor `liftM` andRestrictor

andRestrictor = try proper <|> simple where
  proper = do n <- notRestrictor
              ws
              char '&'
              ws
              a <- andRestrictor
              return $ ProperAndRestrictor n a
  simple = SimpleAndRestrictor `liftM` notRestrictor

notRestrictor = negate <|> posate where
  negate = do char '!'
              a <- innerRestrictor
              return $ NegateRestrictor a
  posate = PosateRestrictor `liftM` innerRestrictor

innerRestrictor = paren <|> atomr where
  paren = do char '('
             ws
             o <- orRestrictor
             ws
             char ')'
             return $ ParenRestrictor o
  atomr = NameRestrictor `liftM` name

eol =   try (string "\r\n")
    <|> try (string "\n\r")
    <|> string "\r"
    <|> string "\n"

-- Whitespace sponges.
spongy_eol = do optional $ (char '#') >> (many $ noneOf "\r\n")
                eol
ws = many (oneOf " ")     -- any number of whitespace
ws_eol = ws >> spongy_eol -- any number of whitespace, otherwise ~empty
el = many $ try ws_eol    -- any number of ~empty lines
