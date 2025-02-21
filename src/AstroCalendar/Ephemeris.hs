module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris) where

import AstroCalendar.Types
import Control.Concurrent.Async
import Data.Map qualified as Map
import Data.Maybe
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import SwissEphemeris qualified as SwE

yearTimes :: Settings -> [UTCTime]
yearTimes settings =
  let year = settingsYear settings
      step = secondsToNominalDiffTime $ 60 * fromIntegral (settingsAccuracy settings)
      beginning = UTCTime (fromGregorian year 1 1) 0
      end = UTCTime (fromGregorian year 12 31) 86400
   in takeWhile (<= end) (iterate (addUTCTime step) beginning)

fullEphemeris :: Settings -> IO (Map.Map SwE.Planet Ephemeris)
fullEphemeris settings = do
  julianDays <- catMaybes <$> traverse SwE.toJulianDay (yearTimes settings)
  timePointEphemeris <- mapConcurrently (\planet -> (planet,) <$> planetaryEphemeris planet julianDays) allPlanets
  pure $ Map.fromList timePointEphemeris

planetaryEphemeris :: SwE.Planet -> [SwE.JulianDayUT1] -> IO Ephemeris
planetaryEphemeris planet times =
  catMaybes
    <$> traverse
      ( \time -> do
          utcTime <- SwE.fromJulianDay time
          position <- SwE.calculateEclipticPosition time planet
          pure $ fmap (utcTime,) (eitherToMaybe position)
      )
      times
  where
    eitherToMaybe = either (const Nothing) Just

parallelEphemeris :: Map.Map SwE.Planet Ephemeris -> TimeSeries Chart
parallelEphemeris = Map.toList . Map.foldrWithKey insertToMap Map.empty
  where
    insertToMap :: SwE.Planet -> Ephemeris -> Map.Map UTCTime Chart -> Map.Map UTCTime Chart
    insertToMap planet ephemeris acc =
      foldr
        ( \(time, position) innerAcc ->
            Map.insertWith Map.union time (Map.singleton planet position) innerAcc
        )
        acc
        ephemeris
