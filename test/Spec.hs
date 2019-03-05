{-# LANGUAGE OverloadedStrings #-}

import           Universum

import           Test.Hspec
import           Text.Parsec
import           Lib

isOk :: IO Bool -> Expectation
isOk action = shouldReturn action True

main :: IO ()
main = hspec $ do
    it "Parsing of record" $ shouldReturn
        (pure $ parse record "" "<div><span>14:00</span><span><em>Kauhea kankkunen</em></span></div>")
        (Right $ Record "14:00" "Kauhea kankkunen")

    it "Parsing of record" $ shouldReturn
        (pure $ parse record "" "<div><span>14:00</span><span>Kauhea kankkunen</span></div>")
        (Right $ Record "14:00" "Kauhea kankkunen")

    it "Parsing of channel" $ shouldReturn
        (pure $ parse channel "" "<div id=\"yle2\"><h3>Yle 2</h3><div><span>02:00</span><span>Yle uutiset</span></div><div><span>03:00</span><span>Yle uutiset</span></div></div>")
        (Right $ Channel "Yle 2" [Record "02:00" "Yle uutiset", Record "03:00" "Yle uutiset"])
        