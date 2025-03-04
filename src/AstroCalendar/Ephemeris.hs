module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris, natalChart) where

import AstroCalendar.Types
import Control.Concurrent.Async
import Data.Map qualified as Map
import Data.Maybe
import Data.Time.Clock (UTCTime (..), addUTCTime, nominalDay, secondsToNominalDiffTime)
import SwissEphemeris qualified as SwE

yearTimes :: EventsSettings -> [UTCTime]
yearTimes settings =
  let step = case settingsPrecision settings of
        Minutely -> secondsToNominalDiffTime 60
        Hourly -> secondsToNominalDiffTime $ 60 * 60
        Daily -> nominalDay
        Monthly -> 30 * nominalDay
        Yearly -> 365.2425 * nominalDay
      (beginning, end) = dateRange settings
   in takeWhile (<= end) (iterate (addUTCTime step) beginning)

natalChart :: SelectionOptions -> UTCTime -> IO Chart
natalChart options utcTime = do
  maybeTime <- SwE.toJulianDay utcTime
  case maybeTime of
    Just time -> do
      Map.fromList
        . catMaybes
        <$> traverse
          ( \planet -> do
              position <- SwE.calculateEclipticPosition time planet
              pure $ fmap (planet,) (eitherToMaybe position)
          )
          (allPlanets options)
    _ -> error $ "Could not convert to julian day: " ++ show utcTime
  where
    eitherToMaybe = either (const Nothing) Just

fullEphemeris :: SelectionOptions -> EventsSettings -> IO (Map.Map SwE.Planet Ephemeris)
fullEphemeris options settings = do
  julianDays <- catMaybes <$> mapConcurrently SwE.toJulianDay (yearTimes settings)
  timePointEphemeris <- mapConcurrently (\planet -> (planet,) <$> planetaryEphemeris planet julianDays) (allPlanets options)
  pure $ Map.fromList timePointEphemeris

planetaryEphemeris :: SwE.Planet -> [SwE.JulianDayUT1] -> IO Ephemeris
planetaryEphemeris planet times = do
  catMaybes
    <$> mapConcurrently
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
