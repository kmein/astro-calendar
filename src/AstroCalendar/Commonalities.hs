{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.Commonalities (commonalitiesJson, chartCommonalities, commonalitiesString) where

import AstroCalendar.Aspect (findAspects)
import AstroCalendar.Types
import Data.Aeson
import Data.List (intersect)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import SwissEphemeris qualified as SwE

type ChartCommonalities = (Map.Map Point SwE.ZodiacSignName, Set.Set Point, Set.Set Aspect)

commonalitiesJson :: ChartCommonalities -> Value
commonalitiesJson (placements, retrogrades, aspects) =
  object
    [ ( "placements",
        toJSON
          $ map
            ( \(point, sign) ->
                object
                  [ ("point", toJSON point),
                    ("sign", zodiacSignToJson sign)
                  ]
            )
          $ Map.toList placements
      ),
      ("aspects", toJSON aspects),
      ("retrogrades", toJSON (Set.map toJSON retrogrades))
    ]

commonalitiesString :: ChartCommonalities -> String
commonalitiesString (placements, retrogrades, aspects) =
  unlines $
    concat
      [ map (\(planet, sign) -> unwords [symbol planet, symbol sign]) $ Map.toList placements,
        map (\planet -> unwords [symbol planet, retrograde]) $ Set.toList retrogrades,
        map aspectString $ Set.toList aspects
      ]

chartCommonalities :: SelectionOptions -> [Chart] -> ChartCommonalities
chartCommonalities options charts = (commonPlacements, commonRetrogrades, commonAspects)
  where
    commonAspects =
      Set.fromList
        . foldl1 intersect
        . map (Map.keys . findAspects options)
        $ charts
    commonPlacements =
      Map.fromList
        . foldl1 intersect
        . map
          ( Map.toList
              . fmap
                ( fromJust
                    . SwE.longitudeZodiacSign
                    . SwE.splitDegreesZodiac
                    . SwE.getEclipticLongitude
                )
          )
        $ charts
    commonRetrogrades =
      Set.fromList
        . foldl1 intersect
        . map (Map.keys . Map.filter (< 0) . fmap (signum . SwE.lngSpeed))
        $ charts
