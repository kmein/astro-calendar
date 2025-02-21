{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Chart where

import AstroCalendar.Angle
import AstroCalendar.Types
import Data.Aeson
import Data.Map qualified as Map
import SwissEphemeris as SwE

chartString :: Chart -> Map.Map Aspect Angle -> String
chartString chart aspects =
  unlines $
    map (\(p, v) -> [symbol p, ' '] ++ showLongitudeComponents (SwE.splitDegreesZodiac $ SwE.getEclipticLongitude v)) (Map.toList chart)
      ++ [[]]
      ++ map
        ( \(a, degreesMinutes -> (degree, minute)) ->
            [ symbol (planet1 a),
              ' ',
              symbol (aspectType a),
              ' ',
              symbol (planet2 a)
            ]
              ++ "\t"
              ++ show degree
              ++ "° "
              ++ show minute
              ++ "ʹ"
        )
        (Map.toList aspects)

chartJson :: Chart -> Map.Map Aspect Angle -> Value
chartJson chart aspects =
  object
    [ "planets"
        .= map
          ( \(p, v) ->
              let longitude = SwE.splitDegreesZodiac (SwE.getEclipticLongitude v)
               in object
                    [ "sign" .= fmap (toJSON . symbol) (SwE.longitudeZodiacSign longitude),
                      "planet" .= toJSON [symbol p],
                      "degrees" .= toJSON (SwE.longitudeDegrees longitude),
                      "minutes" .= toJSON (SwE.longitudeMinutes longitude),
                      "speed" .= SwE.lngSpeed v
                    ]
          )
          (Map.toList chart),
      "aspects"
        .= toJSON
          ( map
              ( \(a, orb) ->
                  object
                    [ "planet1" .= toJSON (symbol (planet1 a)),
                      "planet2" .= toJSON (symbol (planet2 a)),
                      "type" .= toJSON (symbol (aspectType a)),
                      "orb" .= toJSON orb
                    ]
              )
              (Map.toList aspects)
          )
    ]

showLongitudeComponents :: LongitudeComponents -> String
showLongitudeComponents longitude
  | Just sign <- longitudeZodiacSign longitude,
    degrees <- longitudeDegrees longitude,
    minutes <- longitudeMinutes longitude =
      [symbol sign] ++ "\t" ++ show degrees ++ "° " ++ show minutes ++ "ʹ"
  | otherwise = "?"
