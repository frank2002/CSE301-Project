module Cmd where

data Cmd = Go_Down Int | Go_Up | Battle | Search | Check | Quit
  deriving (Show, Read)