{-# LANGUAGE OverloadedStrings #-}
module IrcParser(parseIrc, IrcEvent(..)) where

import Data.Attoparsec.Text
import Data.Text
import Data.Char
import Control.Applicative

data IrcEvent = Message Text Text [Text] deriving (Show, Eq)

newline :: Char -> Bool
newline c = c == '\r' || c == '\n'

message :: Parser Text
message = do
    str <- takeTill newline
    endOfLine <|> endOfInput
    return str

messageName :: Parser Text
messageName = char ':' *> word

word :: Parser Text
word = takeWhile1 (not . isSpace)

messageCommand :: Parser Text
messageCommand = word

messageParameter :: Parser Text
messageParameter = char ':' *> message <|> word

parseIrc :: Parser IrcEvent
parseIrc = do
    name <- option "" messageName
    skipSpace
    comm <- messageCommand
    skipSpace
    param <- messageParameter `sepBy'` skipSpace
    return $ Message name comm param
