{-# LANGUAGE OverloadedStrings #-}

import           Universum

import           Test.Hspec
import           Text.Parsec
import           Lib

isOk :: IO Bool -> Expectation
isOk action = shouldReturn action True

main :: IO ()
main = hspec $
    it "Parsing of record" $ shouldReturn
        (pure $ parse record "" "<div><span>14:00</span><span><em>Kauhea kankkunen</em></span></div>")
        (Right $ Record "14:00" "Kauhea kankkunen")