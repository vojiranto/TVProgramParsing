{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , record
    , Record(..)
    , test
    ) where

import Universum hiding (many, optional)
import Text.Parsec
import Text.Parsec.Char
import Data.Text (pack)

data Record = Record Text Text deriving (Show, Eq)

someFunc :: IO ()
someFunc = putTextLn "someFunc"

test =  parse record "" "<div><span>14:00</span><span><em>Kauhea kankkunen</em></span></div>"

record :: Parsec Text u Record
record = do
    string "<div><span>"
    time <- many $ noneOf "<>"
    string "</span><span>"
    optional $ string "<em>"
    name <- many $ noneOf "<>"
    optional $ string "</em>"
    string "</span></div>"
    pure $ Record (pack time) (pack name)