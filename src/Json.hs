{-# LANGUAGE OverloadedStrings   #-}

module Json where

import Universum
import Data.Aeson

import Types

instance ToJSON Record where
    toJSON (Record time name) =
        object
            [ "timeslot"    .= time
            , "name"        .= name
            ]

instance ToJSON Channel where
    toJSON (Channel channel programming) =
        object
            [ "channel"     .= channel
            , "programming" .= programming
            ]

instance ToJSON Day where
    toJSON (Day channels) = object ["channels" .= channels]