module AstroCalendar.Ephemeris (ephemeris, parallelEphemeris) where

import AstroCalendar.Types
import Control.Monad
import Data.Map qualified as Map
import Data.Maybe
import Data.Time.Calendar (Year, fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import SwissEphemeris qualified as SwE

step :: NominalDiffTime
step = secondsToNominalDiffTime (60 * 60) -- hourly

yearTimes :: Year -> [UTCTime]
yearTimes year =
  let beginning = UTCTime (fromGregorian year 1 1) 0
      end = UTCTime (fromGregorian year 12 31) 86400
   in takeWhile (<= end) (iterate (addUTCTime step) beginning)

ephemeris :: Year -> IO (Map.Map SwE.Planet Ephemeris)
ephemeris year = do
  julianDays <- catMaybes <$> traverse SwE.toJulianDay (yearTimes year)
  Map.fromList <$> traverse (\planet -> (planet,) <$> planetaryEphemeris planet julianDays) allPlanets

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
