{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parsing
    ( record
    , channel
    , content
    , day
    , structTag
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
    structTag "html" $ do
        structTag "head" spaces
        structTag "body" $ do
            divTagHead 
            manyUntil day "</div>"

            
data Day = Day [Channel] deriving (Show, Eq)

day :: Parsec Text u Day
day = do
    divTagHead
    void (structTag "h2" bString)
    Day <$> manyUntil channel "</div>"

data Channel = Channel Text [Record] deriving (Show, Eq)

channel :: Parsec Text u Channel
channel = divTagHead >> Channel <$> channelName <*> manyUntil record "</div>"

channelName :: Parsec Text u Text
channelName = structTag "h3" bString

data Record = Record Text Text deriving (Show, Eq)

record :: Parsec Text u Record
record = structTag "div" (Record <$> textTag "span" bString <*> textTag "span" recordName)   

recordName :: Parsec Text u Text
recordName = textTag "em" bString <|> bString

--------------------------------------------------------------------------------
--                              Common Internal                               --
--------------------------------------------------------------------------------

divTagHead :: Parsec Text u ()
divTagHead = spaces >> void (string "<div" >> skipMany (noneOf ">") >> string ">")

bString :: Parsec Text u Text
bString = pack <$> (many $ noneOf "<")

textTag :: String -> Parsec Text u p -> Parsec Text u p
textTag n = between (string $ "<" <> n <> ">") (string $ "</" <> n <> ">")

structTag :: String -> Parsec Text u p -> Parsec Text u p
structTag n p = spaces >> between (string $ "<" <> n <> ">") (spaces >> string ("</" <> n <> ">")) p

manyUntil :: Parsec Text u a -> String -> Parsec Text u [a]
manyUntil p t = manyTill p (try (spaces >> string t))