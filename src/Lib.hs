{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import           Universum hiding (toStrict)
import           Universum.Exception
import qualified Parsing as P
import qualified Json    as J
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict)
import           Types
import           Text.Parsec
import           Data.Aeson.Encode.Pretty
import           Universum.Lifted.File

someFunc :: IO ()
someFunc = 
    catch (do
        html <- readFile "data/tv.html"
        let Right tvProgram = parse P.content "" html
        B.writeFile "data/sample.json" (toStrict $ encodePretty tvProgram)

    ) (\(err :: SomeException) -> putTextLn $ "Error: " <> show err)
