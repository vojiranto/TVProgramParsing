{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , record
    , channel
    , Record(..)
    , Channel(..)
    ) where

import Universum hiding (many, optional, (<|>))
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
        void $ many $ noneOf "\""
        string "\">"
    ) (string "</div>"
    ) (do
        name    <- channelName
        records <- many record
        pure $ Channel name records
    )

channelName :: Parsec Text u Text
channelName = do
    name  <- between (string "<h3>") (string "</h3>") (many $ noneOf "<>")
    pure $ pack name

data Record = Record Text Text deriving (Show, Eq)

record :: Parsec Text u Record
record = between (string "<div>") (string "</div>") $ do
    time <- between (string "<span>") (string "</span>") (many $ noneOf "<>")
    name <- between (string "<span>") (string "</span>") recordName
    pure $ Record (pack time) name

recordName :: Parsec Text u Text
recordName =
    (do
        name <- between (string "<em>") (string "</em>") $ many (noneOf "<>")
        pure $ pack name
    ) <|> (do
        name <- many $ noneOf "<>"
        pure $ pack name
    )  