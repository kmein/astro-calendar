module AstroCalendar.Ephemeris (fullEphemeris, parallelEphemeris, natalChart) where

import AstroCalendar.Angle (longitudeToEclipticPosition, midpoint)
import AstroCalendar.Types
import Control.Concurrent.Async
import Control.Monad (guard)
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

fullEphemeris :: SelectionOptions -> EventsSettings -> IO (Map.Map Point Ephemeris)
fullEphemeris options settings = do
  julianDays <- catMaybes <$> mapConcurrently SwE.toJulianDay (yearTimes settings)
  timePointEphemeris <- mapConcurrently (\planet -> (Planet planet,) <$> planetaryEphemeris planet julianDays) (allPlanets options)
  let midpointEphemeris
        | EbertinPlanets <- planetSelection options =
            [ (Midpoint p1 p2, Map.toList (Map.intersectionWith midpoint (Map.fromList ephemeris1) (Map.fromList ephemeris2)))
              | (p1, ephemeris1) <- timePointEphemeris,
                (p2, ephemeris2) <- timePointEphemeris
            ]
        | otherwise = []
  pure $ Map.fromList (timePointEphemeris <> midpointEphemeris)

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

parallelEphemeris :: Map.Map Point Ephemeris -> TimeSeries Chart
parallelEphemeris = Map.toList . Map.foldrWithKey insertToMap Map.empty
  where
    insertToMap :: Point -> Ephemeris -> Map.Map UTCTime Chart -> Map.Map UTCTime Chart
    insertToMap point ephemeris acc =
      foldr
        ( \(time, position) innerAcc ->
            Map.insertWith Map.union time (Map.singleton point position) innerAcc
        )
        acc
        ephemeris
