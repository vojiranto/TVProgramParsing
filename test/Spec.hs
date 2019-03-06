{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Universum

import           Test.Hspec
import           Text.Parsec
import           Text.RawString.QQ
import           Lib

isOk :: IO Bool -> Expectation
isOk action = shouldReturn action True

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Parsing of record 1" $ shouldReturn
            (pure $ parse record "" " <div><span>14:00</span><span><em>Kauhea kankkunen</em></span></div>")
            (Right $ Record "14:00" "Kauhea kankkunen")

        it "Parsing of record 2" $ shouldReturn
            (pure $ parse record "" " <div><span>14:00</span><span>Kauhea kankkunen</span></div>")
            (Right $ Record "14:00" "Kauhea kankkunen")

        it "Parsing of day" $ shouldReturn
            (pure $ parse day "" [r|
                <div class="day" id="monday">
                    <h2>Monday</h2>
                    <div id="yle1">
                        <h3>Yle 1</h3>
                            <div><span>02:00</span><span>Yle uutiset</span></div>
                    </div>
                </div>
            |])
            (Right $ Day [Channel "Yle 1" [Record "02:00" "Yle uutiset"]])

        it "Parsing of channel" $ shouldReturn
            (pure $ parse channel "" [r|
                <div id="yle2">
                    <h3>Yle 2</h3>
                    <div><span>02:00</span><span>Yle uutiset</span></div>
                    <div><span>03:00</span><span>Yle uutiset</span></div>
                </div>
            |])
            (Right $ Channel "Yle 2" [Record "02:00" "Yle uutiset", Record "03:00" "Yle uutiset"])
    
        it "Parsing of record name1" $ shouldReturn
            (pure $ parse recordName "" "Kauhea kankkunen")
            (Right "Kauhea kankkunen")

        it "Parsing of record name2" $ shouldReturn
            (pure $ parse recordName "" "<em>Kauhea kankkunen</em>")
            (Right "Kauhea kankkunen")