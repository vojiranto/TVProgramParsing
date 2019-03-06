{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    , record
    , channel
    , day
    , tag
    , bString
    , recordName
    , Record(..)
    , Channel(..)
    , Day (..)
    ) where

import Universum hiding (many, optional, (<|>), try)
import qualified Universum as U
import Text.Parsec 
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Text (pack)

someFunc :: IO ()
someFunc = putTextLn "someFunc"

content :: Parsec Text u [Day]
content =
    tag "html" $ do
        tag "head" spaces
        tag "body" $ do
            string "<div id=\"content\">"
            manyUntil day "</div>"

data Day = Day [Channel] deriving (Show, Eq)

day :: Parsec Text u Day
day = do
    spaces >> string "<div class=\"day\" id=\"" >> skipMany (noneOf "\"") >> string "\">"
    void (tag "h2" bString)
    Day <$> manyUntil channel "</div>"

data Channel = Channel Text [Record] deriving (Show, Eq)

channel :: Parsec Text u Channel
channel = do
    spaces >> string "<div id=\"" >> skipMany (noneOf "\"") >> string "\">"
    Channel <$> channelName <*> manyUntil record "</div>"

channelName :: Parsec Text u Text
channelName = tag "h3" bString

data Record = Record Text Text deriving (Show, Eq)

record :: Parsec Text u Record
record = tag "div" (Record <$> tag "span" bString <*> tag "span" recordName)   

recordName :: Parsec Text u Text
recordName = tag "em" bString <|> bString

--------------------------------------------------------------------------------
--                              Common Internal                               --
--------------------------------------------------------------------------------

bString :: Parsec Text u Text
bString = pack <$> (many $ noneOf "<")


tag :: String -> Parsec Text u p -> Parsec Text u p
tag n p = do
    spaces
    between (string $ "<" <> n <> ">") (string $ "</" <> n <> ">") p

manyUntil :: Parsec Text u a -> String -> Parsec Text u [a]
manyUntil p t = manyTill p (try (spaces >> string t))