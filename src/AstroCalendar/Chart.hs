{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Chart (chartJson, chartString) where

import AstroCalendar.Angle
import AstroCalendar.Types
import Data.Aeson
import Data.List (foldl', intercalate, sortOn)
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

positionString :: Planet -> [SwE.EclipticPosition] -> String
positionString planet positions =
  [symbol planet, '\t']
    ++ intercalate
      "\t"
      ( map
          ( \x ->
              showLongitudeComponents
                (SwE.splitDegreesZodiac (SwE.getEclipticLongitude x))
                ++ if SwE.lngSpeed x < 0 then [' ', retrograde] else []
          )
          positions
      )

chartString :: [Chart] -> Map.Map Aspect Angle -> String
chartString charts aspects =
  unlines $
    concat
      [ map (uncurry positionString) $ combineMaps charts,
        [[]],
        map (uncurry aspectString) (sortOn (abs . snd) $ Map.toList aspects)
      ]

combineMaps :: (Ord k) => [Map.Map k v] -> [(k, [v])]
combineMaps = Map.toList . foldl' insertIntoMap Map.empty
  where
    insertIntoMap acc m = Map.unionWith (++) acc (Map.map (: []) m)

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

chartJson :: [Chart] -> Map.Map Aspect Angle -> Value
chartJson charts aspects =
  object
    [ "charts" .= map (map (uncurry positionJson) . Map.toList) charts,
      "aspects" .= map (uncurry aspectJson) (sortOn (abs . snd) $ Map.toList aspects)
    ]

showLongitudeComponents :: LongitudeComponents -> String
showLongitudeComponents longitude
  | Just sign <- longitudeZodiacSign longitude,
    degrees <- longitudeDegrees longitude,
    minutes <- longitudeMinutes longitude =
      [symbol sign] ++ " " ++ show degrees ++ "° " ++ show minutes ++ "ʹ"
  | otherwise = "?"
