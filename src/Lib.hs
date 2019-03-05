{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Universum as U
import Text.Parsec 
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Text (pack)

someFunc :: IO ()
someFunc = putTextLn "someFunc"

data Channel = Channel Text [Record] deriving (Show, Eq)

channel :: Parsec Text u Channel
channel = do
    spaces >> string "<div id=\"" >> skipMany (noneOf "\"") >> string "\">"

    name    <- channelName
    records <- manyTill record (try (string "</div>"))
    pure $ Channel name records

channelName :: Parsec Text u Text
channelName = spaces >> tag "h3" bString

data Record = Record Text Text deriving (Show, Eq)

record :: Parsec Text u Record
record = 
    spaces >> tag "div" (Record <$> tag "span" bString <*> tag "span" recordName)   

recordName :: Parsec Text u Text
recordName = tag "em" bString <|> bString

bString :: Parsec Text u Text
bString = pack <$> (many $ noneOf "<")

tag :: String -> Parsec Text u p -> Parsec Text u p
tag n = between (string $ "<" <> n <> ">") (string $ "</" <> n <> ">")
