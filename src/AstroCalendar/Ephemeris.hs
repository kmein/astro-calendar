{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris, natalChart) where

import AstroCalendar.Types
import Control.Concurrent.Async
import Data.Fixed (mod') -- To keep angles in the [0,360) range
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
    pure $ midpoint <$> ep <*> eq

midpoint :: SwE.EclipticPosition -> SwE.EclipticPosition -> SwE.EclipticPosition
midpoint a b =
  let -- Convert longitude to radians
      degToRad x = x * pi / 180
      radToDeg x = x * 180 / pi

      -- Convert longitudes to Cartesian coordinates
      x1 = cos (degToRad (SwE.lng a))
      y1 = sin (degToRad (SwE.lng a))
      x2 = cos (degToRad (SwE.lng b))
      y2 = sin (degToRad (SwE.lng b))

      -- Average Cartesian coordinates
      xMid = (x1 + x2) / 2
      yMid = (y1 + y2) / 2

      -- Convert back to longitude (handling quadrant issues)
      midLng = mod' (radToDeg (atan2 yMid xMid)) 360
   in SwE.EclipticPosition
        { SwE.lng = midLng,
          SwE.lat = (SwE.lat a + SwE.lat b) / 2,
          SwE.distance = (SwE.distance a + SwE.distance b) / 2,
          SwE.lngSpeed = (SwE.lngSpeed a + SwE.lngSpeed b) / 2,
          SwE.latSpeed = (SwE.latSpeed a + SwE.latSpeed b) / 2,
          SwE.distSpeed = (SwE.distSpeed a + SwE.distSpeed b) / 2
        }

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
