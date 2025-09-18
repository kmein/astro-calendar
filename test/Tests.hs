module Main where

import AstroCalendar.Aspect
import AstroCalendar.Chart
import AstroCalendar.Ephemeris
import AstroCalendar.Types
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time (UTCTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import SwissEphemeris (GeographicPosition (..))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

parseISO8601 :: String -> Maybe UTCTime
parseISO8601 isoString =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" isoString :: Maybe UTCTime

tests :: TestTree
tests = testGroup "All Tests" []

birthCharts :: TestTree
birthCharts =
  testGroup
    "Birth charts"
    [ testCase "Goethe's birth chart" $ do
        -- https://www.astro-seek.com/birth-chart/johann-wolfgang-von-goethe-horoscope
        let selectionOptions =
              SelectionOptions
                { planetSelection = ModernPlanets,
                  aspectTypeSelection = AllAspectTypes,
                  orbSelection = AstroDienst,
                  position = Just GeographicPosition {geoLat = 50.12, geoLng = 8.68},
                  tzSelection = Nothing
                }
            utcTime = fromJust $ parseISO8601 "1749-08-28 11:55"
        chart <- natalChart selectionOptions utcTime
        let aspects = findAspects selectionOptions chart
        let gotJson = chartJson [chart] aspects
        neededJson <- decodeFileStrict "test_data/goethe_chart.json" :: IO (Maybe Value)
        neededJson @=? Just gotJson
    ]
