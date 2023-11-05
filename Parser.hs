module Parser
  ( runParser
  , parseCmd
  , parseInput
  ) where

import           Definition

import           Data.Char
import           Data.Maybe

import           Control.Applicative

newtype Parser tok a = Parser
  { runParser :: [tok] -> Maybe (a, [tok])
  }

instance Monad (Parser tok)
 where
  return x = Parser (\ts -> Just (x, ts))
  p >>= f =
    Parser
      (\ts ->
         case runParser p ts of
           Nothing       -> Nothing
           Just (x, ts') -> runParser (f x) ts')

instance Functor (Parser tok)
 where
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok)
 where
  pure     = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

instance Alternative (Parser tok)
 where
  empty = Parser (\ts -> Nothing)
  p1 <|> p2 =
    Parser
      (\ts ->
         case runParser p1 ts of
           Just (x, ts') -> Just (x, ts')
           Nothing       -> runParser p2 ts)

token :: Parser tok tok
token =
  Parser $ \ts ->
    case ts of
      []      -> Nothing
      (t:ts') -> Just (t, ts')

sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t
    then return t
    else empty

match :: String -> Parser String String
match s = sat (\s' -> map toLower s == map toLower s')

cardinal :: Parser String Int
cardinal = do
  (match "one" >> return 1)
    <|> (match "two" >> return 2)
    <|> (match "three" >> return 3)
    <|> (match "four" >> return 4)
    <|> (match "five" >> return 5)
    <|> (match "six" >> return 6)
    <|> (match "seven" >> return 7)
    <|> (match "eight" >> return 8)
    <|> (match "nine" >> return 9)

ordinal :: Parser String Int
ordinal = do
  (match "first" >> return 1)
    <|> (match "second" >> return 2)
    <|> (match "third" >> return 3)
    <|> (match "fourth" >> return 4)
    <|> (match "fifth" >> return 5)
    <|> (match "sixth" >> return 6)
    <|> (match "seventh" >> return 7)
    <|> (match "eighth" >> return 8)
    <|> (match "ninth" >> return 9)

parseCmd :: Parser String Cmd
parseCmd =
  parseMoveDown
    <|> parseMoveUp
    <|> parseBattle
    <|> parseSearch
    <|> parseCheck
    <|> parseQuit

parseMoveDown :: Parser String Cmd
parseMoveDown = do
  _ <- match "move" <|> match "go"
  _ <- match "down"
  _ <- match "to"
  (do
     match "child"
     n <- cardinal
     return (Go_Down n))
    <|> (do
           match "the"
           n <- ordinal
           match "child"
           return (Go_Down n))

parseMoveUp :: Parser String Cmd
parseMoveUp = do
  match "move" <|> match "go"
  match "up"
  return Go_Up

parseBattle :: Parser String Cmd
parseBattle = do
  match "battle"
  return Battle

parseSearch :: Parser String Cmd
parseSearch = do
  match "search"
  return Search

parseCheck :: Parser String Cmd
parseCheck = do
  match "check"
  return Check

parseQuit :: Parser String Cmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit

parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s =
  case runParser p (words s) of
    Just (x, ts') ->
      if null ts'
        then return x
        else fail "parseInput: some tokens left"
    Nothing -> fail "parseInput: failed to parse"
