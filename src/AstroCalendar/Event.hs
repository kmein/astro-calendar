{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Event (astrologicalEvents, AstrologicalEvents) where

import AstroCalendar.Angle (Angle)
import AstroCalendar.Aspect
import AstroCalendar.Ephemeris (natalChart, parallelEphemeris)
import AstroCalendar.Types
import Control.Arrow (second)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map qualified as Map
import Data.Maybe
import SwissEphemeris qualified as SwE

type AstrologicalEvents = (Maybe [RetrogradeEvent], Maybe [SignEvent], Maybe [AspectEvent], Maybe [AspectEvent])

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

aspectEvents :: TimeSeries (Map.Map Aspect Angle) -> [AspectEvent]
aspectEvents aspects =
  concatMap findOccurrences allAspects
  where
    findOccurrences :: Aspect -> [AspectEvent]
    findOccurrences aspect = mapMaybe period $ chunkTimeSeries (Map.member aspect) aspects
      where
        period :: TimeSeries (Map.Map Aspect Angle) -> Maybe AspectEvent
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

astrologicalEvents :: Settings -> Map.Map SwE.Planet Ephemeris -> IO AstrologicalEvents
astrologicalEvents settings planetEphemeris = do
  (if withRetrograde settings then Just retrogradePeriods else Nothing,if withSigns settings then Just signPeriods else Nothing,if withAspects settings then Just aspectPeriods else Nothing,)
    <$> transitPeriods (transitsTo settings)
  where
    retrogradePeriods = concat $ Map.elems $ Map.mapWithKey retrogradeEvents planetEphemeris
    signPeriods = concat $ Map.elems $ Map.mapWithKey signEvent planetEphemeris
    aspectPeriods = aspectEvents $ map (second (findAspects Nothing)) $ parallelEphemeris planetEphemeris
    transitPeriods = \case
      Just birthTime -> do
        natal <- natalChart birthTime
        pure $
          Just $
            aspectEvents $
              map (second (findAspects (Just natal))) $
                parallelEphemeris planetEphemeris
      Nothing -> pure Nothing
