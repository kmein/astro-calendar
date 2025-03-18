{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module AstroCalendar.Event (astrologicalEvents, AstrologicalEvents) where

import AstroCalendar.Angle (Angle)
import AstroCalendar.Aspect
import AstroCalendar.Eclipse
import AstroCalendar.Ephemeris (fullEphemeris, natalChart, parallelEphemeris)
import AstroCalendar.Types
import Control.Arrow (second)
import Control.Parallel (par)
import Data.Set qualified as Set
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe
import SwissEphemeris qualified as SwE

type AstrologicalEvents = (Maybe [RetrogradeEvent], Maybe [SignEvent], Maybe [AspectEvent NatalAspect], Maybe [AspectEvent TransitAspect], Maybe [EclipseEvent])

signEvent :: Point -> Ephemeris -> [SignEvent]
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

aspectEvents :: TimeSeries (Map.Map Aspect Angle) -> [AspectEvent NatalAspect]
aspectEvents aspects =
  concatMap findOccurrences $ Set.toList $ Set.fromList $ concatMap (Map.keys . snd) aspects
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

transitEvents :: TimeSeries (Map.Map Aspect Angle) -> [AspectEvent TransitAspect]
transitEvents transits =
  concatMap findOccurrences $ Set.toList $ Set.fromList $ concatMap (Map.keys . snd) transits
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

retrogradeEvents :: Point -> Ephemeris -> [RetrogradeEvent]
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

astrologicalEvents :: SelectionOptions -> EventsSettings -> IO AstrologicalEvents
astrologicalEvents options settings = do
  let ephemerisNeeded =
        withAspects settings
          || withRetrograde settings
          || withSigns settings
          || isJust (transitsTo settings)
  planetEphemeris <- if ephemerisNeeded then fullEphemeris options settings else pure Map.empty
  let retrogradePeriods
        | withRetrograde settings = Just $ concat $ Map.elems $ Map.mapWithKey retrogradeEvents planetEphemeris
        | otherwise = Nothing
      signPeriods
        | withSigns settings = Just $ concat $ Map.elems $ Map.mapWithKey signEvent planetEphemeris
        | otherwise = Nothing
      aspectPeriods
        | withAspects settings = Just $ aspectEvents $ map (second (findAspects options)) $ parallelEphemeris planetEphemeris
        | otherwise = Nothing
      transitPeriods = \case
        Just birthTime -> do
          natal <- natalChart options birthTime
          pure $
            Just $
              transitEvents $
                map (second (findTransits options natal)) $
                  parallelEphemeris planetEphemeris
        Nothing -> pure Nothing
  ts <- transitPeriods (transitsTo settings)
  eclipses <- if withEclipses settings then Just <$> findEclipses settings else pure Nothing
  return
    $ ( retrogradePeriods `par`
          signPeriods `par`
            aspectPeriods `par`
              eclipses `par`
                ts `par`
                  ()
      )
    `seq` ( retrogradePeriods,
            signPeriods,
            aspectPeriods,
            ts,
            eclipses
          )
