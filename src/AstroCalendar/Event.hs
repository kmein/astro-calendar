{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module AstroCalendar.Event (astrologicalEvents, AstrologicalEvents) where

import AstroCalendar.Angle (Angle)
import AstroCalendar.Aspect
import AstroCalendar.Ephemeris (natalChart, parallelEphemeris)
import AstroCalendar.Types
import AstroCalendar.Eclipse
import Control.Arrow (second)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe
import SwissEphemeris qualified as SwE

type AstrologicalEvents = (Maybe [RetrogradeEvent], Maybe [SignEvent], Maybe [AspectEvent NatalAspect], Maybe [AspectEvent TransitAspect], Maybe [EclipseEvent])

signEvent :: SwE.Planet -> Ephemeris -> [SignEvent]
signEvent planet =
  map groupToRange . chunkTimeSeries signFromPosition
  where
    groupToRange :: Ephemeris -> SignEvent
    groupToRange group =
      let times = timeSeriesTimes group
       in SignEvent
            { sign = signFromPosition $ snd $ head group,
              planet = planet,
              signStartTime = minimum times,
              signEndTime = maximum times
            }

    signFromPosition :: SwE.EclipticPosition -> Maybe SwE.ZodiacSignName
    signFromPosition = SwE.longitudeZodiacSign . SwE.splitDegreesZodiac . SwE.getEclipticLongitude

aspectEvents :: PlanetSelection -> TimeSeries (Map.Map Aspect Angle) -> [AspectEvent NatalAspect]
aspectEvents planetSelection aspects =
  concatMap findOccurrences (allAspects planetSelection)
  where
    findOccurrences :: Aspect -> [AspectEvent NatalAspect]
    findOccurrences aspect = mapMaybe period $ chunkTimeSeries (Map.member aspect) aspects
      where
        period :: TimeSeries (Map.Map Aspect Angle) -> Maybe (AspectEvent NatalAspect)
        period aspectTimes
          | (_, tAspects) : _ <- aspectTimes,
            times <- timeSeriesTimes aspectTimes,
            Map.member aspect tAspects =
              Just
                AspectEvent
                  { aspect = aspect,
                    aspectStartTime = minimum times,
                    aspectEndTime = maximum times,
                    aspectExactTime = getTime (minimumBy (compare `on` ((Map.! aspect) . getValue)) aspectTimes)
                  }
          | otherwise = Nothing

transitEvents :: PlanetSelection -> TimeSeries (Map.Map Aspect Angle) -> [AspectEvent TransitAspect]
transitEvents planetSelection transits =
  concatMap findOccurrences (allTransits planetSelection)
  where
    findOccurrences :: Aspect -> [AspectEvent TransitAspect]
    findOccurrences transit = mapMaybe period $ chunkTimeSeries (Map.member transit) transits
      where
        period :: TimeSeries (Map.Map Aspect Angle) -> Maybe (AspectEvent TransitAspect)
        period transitTimes
          | (_, tTransits) : _ <- transitTimes,
            times <- timeSeriesTimes transitTimes,
            length transitTimes > 1,
            Map.member transit tTransits =
              Just
                AspectEvent
                  { aspect = transit,
                    aspectStartTime = minimum times,
                    aspectEndTime = maximum times,
                    aspectExactTime = getTime (minimumBy (compare `on` ((Map.! transit) . getValue)) transitTimes)
                  }
          | otherwise = Nothing

retrogradeEvents :: SwE.Planet -> Ephemeris -> [RetrogradeEvent]
retrogradeEvents planet =
  mapMaybe groupToRange . chunkTimeSeries direction
  where
    groupToRange :: TimeSeries SwE.EclipticPosition -> Maybe RetrogradeEvent
    groupToRange group
      | (_, lng) : _ <- group,
        direction lng < 0 =
          let times = timeSeriesTimes group
           in Just
                RetrogradeEvent
                  { retrogradePlanet = planet,
                    retrogradeStartTime = minimum times,
                    retrogradeEndTime = maximum times
                  }
      | otherwise = Nothing
    direction = signum . SwE.lngSpeed

astrologicalEvents :: PlanetSelection -> EventsSettings -> Map.Map SwE.Planet Ephemeris -> IO AstrologicalEvents
astrologicalEvents planetSelection settings planetEphemeris = do
  ts <- transitPeriods (transitsTo settings)
  eclipses <- findEclipses settings
  return
    ( if withRetrograde settings then Just retrogradePeriods else Nothing,
      if withSigns settings then Just signPeriods else Nothing,
      if withAspects settings then Just aspectPeriods else Nothing,
      ts,
      if withEclipses settings then Just eclipses else Nothing
    )
  where
    retrogradePeriods = concat $ Map.elems $ Map.mapWithKey retrogradeEvents planetEphemeris
    signPeriods = concat $ Map.elems $ Map.mapWithKey signEvent planetEphemeris
    aspectPeriods = aspectEvents planetSelection $ map (second (findAspects planetSelection)) $ parallelEphemeris planetEphemeris
    transitPeriods = \case
      Just birthTime -> do
        natal <- natalChart planetSelection birthTime
        pure $
          Just $
            transitEvents planetSelection $
              map (second (findTransits planetSelection natal)) $
                parallelEphemeris planetEphemeris
      Nothing -> pure Nothing
