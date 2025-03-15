module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris, natalChart) where

import AstroCalendar.Types
import Control.Concurrent.Async
import Control.Monad (guard)
import Data.Fixed (mod')
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

-- | Calculate the midpoint between two EclipticPosition values
midpoint :: SwE.EclipticPosition -> SwE.EclipticPosition -> SwE.EclipticPosition
midpoint pos1 pos2 =
  SwE.EclipticPosition
    { SwE.lng = midpointLongitude (SwE.lng pos1) (SwE.lng pos2),
      SwE.lat = avg (SwE.lat pos1) (SwE.lat pos2),
      SwE.distance = avg (SwE.distance pos1) (SwE.distance pos2),
      SwE.lngSpeed = avg (SwE.lngSpeed pos1) (SwE.lngSpeed pos2),
      SwE.latSpeed = avg (SwE.latSpeed pos1) (SwE.latSpeed pos2),
      SwE.distSpeed = avg (SwE.distSpeed pos1) (SwE.distSpeed pos2)
    }
  where
    avg a b = (a + b) / 2
    midpointLongitude lng1 lng2
      | abs (lng1 - lng2) <= 180 = avg lng1 lng2
      | lng1 < lng2 = avg (lng1 + 360) lng2 `mod'` 360
      | otherwise = avg lng1 (lng2 + 360) `mod'` 360

-- | Convert a longitude value into an EclipticPosition with default values
longitudeToEclipticPosition :: Double -> SwE.EclipticPosition
longitudeToEclipticPosition lng = SwE.EclipticPosition {SwE.lng = lng, SwE.lat = 0.0, SwE.distance = 0.0, SwE.lngSpeed = 0.0, SwE.latSpeed = 0.0, SwE.distSpeed = 0.0}

natalChart :: SelectionOptions -> UTCTime -> IO Chart
natalChart options utcTime = do
  maybeTime <- SwE.toJulianDay utcTime
  case maybeTime of
    Just time -> do
      angles <- case position options of
        Just position -> do
          cusps <- SwE.calculateCusps SwE.Placidus time position
          let angleCusps = SwE.angles cusps
          pure $
            Map.fromList
              [ (Ascendant, longitudeToEclipticPosition (SwE.ascendant angleCusps)),
                (MediumCaeli, longitudeToEclipticPosition (SwE.mc angleCusps))
              ]
        Nothing -> pure Map.empty
      planets <-
        Map.fromList . catMaybes
          <$> traverse
            ( \planet -> do
                position <- SwE.calculateEclipticPosition time planet
                pure $ (planet,) <$> eitherToMaybe position
            )
            (allPlanets options)
      let planetsAndAngles = Map.mapKeys Planet planets `Map.union` Map.mapKeys AnglePoint angles
          theMidpoints
            | EbertinPlanets <- planetSelection options =
                Map.fromList $ do
                  (p1, x1) <- Map.toList planetsAndAngles
                  (p2, x2) <- Map.toList planetsAndAngles
                  guard $ p1 < p2
                  pure (Midpoint p1 p2, midpoint x1 x2)
            | otherwise = Map.empty
      pure $ planetsAndAngles `Map.union` theMidpoints
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
            Map.insertWith Map.union time (Map.singleton (Planet planet) position) innerAcc
        )
        acc
        ephemeris
