{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Chart (chartJson, chartString) where

import AstroCalendar.Angle
import AstroCalendar.Types
import Data.Aeson
import Data.Map qualified as Map
import SwissEphemeris as SwE

aspectString :: Aspect -> Angle -> String
aspectString aspect (degreesMinutes -> (degree, minute)) =
  [ symbol (planet1 aspect),
    ' ',
    symbol (aspectType aspect),
    ' ',
    symbol (planet2 aspect)
  ]
    ++ "\t"
    ++ show degree
    ++ "° "
    ++ show minute
    ++ "ʹ"

positionString :: Planet -> SwE.EclipticPosition -> String
positionString planet position =
  [symbol planet, ' ']
    ++ showLongitudeComponents
      (SwE.splitDegreesZodiac $ SwE.getEclipticLongitude position)

chartString :: Chart -> Map.Map Aspect Angle -> String
chartString chart aspects =
  unlines $
    concat
      [ map (uncurry positionString) (Map.toList chart),
        [[]],
        map (uncurry aspectString) (Map.toList aspects)
      ]

aspectJson :: Aspect -> Angle -> Value
aspectJson aspect orb =
  object
    [ "planet1" .= planetToJson (planet1 aspect),
      "planet2" .= planetToJson (planet2 aspect),
      "type" .= aspectType aspect,
      "orb" .= orb
    ]

positionJson :: Planet -> EclipticPosition -> Value
positionJson planet position =
  object
    [ "sign" .= fmap zodiacSignToJson (SwE.longitudeZodiacSign longitude),
      "planet" .= planetToJson planet,
      "degrees" .= SwE.longitudeDegrees longitude,
      "minutes" .= SwE.longitudeMinutes longitude,
      "retrograde" .= (SwE.lngSpeed position < 0)
    ]
  where
    longitude = SwE.splitDegreesZodiac (SwE.getEclipticLongitude position)

chartJson :: Chart -> Map.Map Aspect Angle -> Value
chartJson chart aspects =
  object
    [ "planets" .= map (uncurry positionJson) (Map.toList chart),
      "aspects" .= map (uncurry aspectJson) (Map.toList aspects)
    ]

showLongitudeComponents :: LongitudeComponents -> String
showLongitudeComponents longitude
  | Just sign <- longitudeZodiacSign longitude,
    degrees <- longitudeDegrees longitude,
    minutes <- longitudeMinutes longitude =
      [symbol sign] ++ "\t" ++ show degrees ++ "° " ++ show minutes ++ "ʹ"
  | otherwise = "?"
