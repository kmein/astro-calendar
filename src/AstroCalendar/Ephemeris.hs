{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris, natalChart) where

import AstroCalendar.Angle
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
              position <- eclipticPosition time planet
              pure $ fmap (planet,) (eitherToMaybe position)
          )
          (allPlanetsOrMidpoints options)
    _ -> error $ "Could not convert to julian day: " ++ show utcTime
  where
    eitherToMaybe = either (const Nothing) Just

fullEphemeris :: SelectionOptions -> EventsSettings -> IO (Map.Map PlanetOrMidpoint Ephemeris)
fullEphemeris options settings = do
  julianDays <- catMaybes <$> mapConcurrently SwE.toJulianDay (yearTimes settings)
  timePointEphemeris <- mapConcurrently (\planet -> (planet,) <$> planetaryEphemeris planet julianDays) (allPlanetsOrMidpoints options)
  pure $ Map.fromList timePointEphemeris

eclipticPosition :: SwE.JulianDayUT1 -> PlanetOrMidpoint -> IO (Either String SwE.EclipticPosition)
eclipticPosition time = \case
  Single p -> SwE.calculateEclipticPosition time p
  Midpoint p q -> do
    ep <- SwE.calculateEclipticPosition time p
    eq <- SwE.calculateEclipticPosition time q
    pure $ do
      ep' <- ep
      eq' <- eq
      -- hacky
      pure ep' {SwE.lng = degrees $ midpoint (Angle $ SwE.lng ep') (Angle $ SwE.lng eq')}

planetaryEphemeris :: PlanetOrMidpoint -> [SwE.JulianDayUT1] -> IO Ephemeris
planetaryEphemeris planet times = do
  catMaybes
    <$> mapConcurrently
      ( \time -> do
          utcTime <- SwE.fromJulianDay time
          position <- eclipticPosition time planet
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
            Map.insertWith Map.union time (Map.singleton (Single planet) position) innerAcc
        )
        acc
        ephemeris
