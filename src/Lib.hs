{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , record
    , channel
    , tag
    , bString
    , recordName
    , Record(..)
    , Channel(..)
    ) where

import Universum hiding (many, optional, (<|>), try)
import Text.Parsec 
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Text (pack)

someFunc :: IO ()
someFunc = putTextLn "someFunc"

data Channel = Channel Text [Record] deriving (Show, Eq)

channel :: Parsec Text u Channel
channel = between (do
        string "<div id=\""
        skipMany $ noneOf "\""
        string "\">"
    ) (string "</div>"
    ) (do
        name    <- channelName
        records <- many record
        pure $ Channel name records
    )

channelName :: Parsec Text u Text
channelName = tag "h3" bString

data Record = Record Text Text deriving (Show, Eq)

record :: Parsec Text u Record
record = tag "div" (Record <$> tag "span" bString <*> tag "span" recordName)   

recordName :: Parsec Text u Text
recordName = between (string "<em>") (string "</em>") bString <|> bString

bString :: Parsec Text u Text
bString = pack <$> (many $ noneOf "<")

tag :: String -> Parsec Text u p -> Parsec Text u p
tag n = between (string $ "<" <> n <> ">") (string $ "</" <> n <> ">")