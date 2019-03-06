module Types
    ( Record(..)
    , Channel(..)
    , Day (..)
    ) where

import Universum

data Day     = Day [Channel] deriving (Show, Eq)
data Channel = Channel Text [Record] deriving (Show, Eq)
data Record  = Record Text Text deriving (Show, Eq)
