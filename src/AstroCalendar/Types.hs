{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Types
  ( AspectType (..),
    Chart,
    Ephemeris,
    Aspect (..),
    TimeSeries,
    allAspects,
    allAspectTypes,
    allPlanets,
    chunkTimeSeries,
    timeSeriesTimes,
    getTime,
    getValue,
    AspectEvent (..),
    SignEvent (..),
    RetrogradeEvent (..),
    IsEvent (..),
  )
where

import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Data.Time.Clock
import SwissEphemeris (EclipticPosition, LongitudeComponents (..), Planet (..), ZodiacSignName (..))

class Symbol a where
  symbol :: a -> Char

instance Symbol AspectType where
  symbol = \case
    Conjunction -> '☌'
    Sextile -> '⚹'
    Square -> '□'
    Trine -> '△'
    Opposition -> '☍'

instance Symbol ZodiacSignName where
  symbol = \case
    Aries -> '♈'
    Taurus -> '♉'
    Gemini -> '♊'
    Cancer -> '♋'
    Leo -> '♌'
    Virgo -> '♍'
    Libra -> '♎'
    Scorpio -> '♏'
    Sagittarius -> '♐'
    Capricorn -> '♑'
    Aquarius -> '♒'
    Pisces -> '♓'

instance Symbol Planet where
  symbol = \case
    Mercury -> '☿'
    Venus -> '♀'
    Mars -> '♂'
    Jupiter -> '♃'
    Saturn -> '♄'
    Uranus -> '♅'
    Neptune -> '♆'
    Pluto -> '⯓'
    Moon -> '☽'
    Sun -> '☉'
    _ -> '\xfffd' -- replacement character

retrograde :: Char
retrograde = '℞'

data Aspect = Aspect
  { planet1 :: Planet,
    planet2 :: Planet,
    aspectType :: AspectType
  }
  deriving (Ord)

instance Eq Aspect where
  a1 == a2 =
    aspectType a1 == aspectType a2
      && ( (planet1 a1 == planet1 a2 && planet2 a1 == planet2 a2)
             || (planet1 a1 == planet2 a2 && planet2 a1 == planet1 a2)
         )

allAspects :: [Aspect]
allAspects =
  [ Aspect p1 p2 t
    | p1 <- allPlanets,
      p2 <- allPlanets,
      p1 < p2,
      t <- allAspectTypes
  ]

allAspectTypes :: [AspectType]
allAspectTypes = [Conjunction, Sextile, Square, Trine, Opposition]

allPlanets :: [Planet]
allPlanets = [Sun .. Pluto]

type TimeSeries a = [(UTCTime, a)]

chunkTimeSeries :: (Eq b) => (a -> b) -> TimeSeries a -> [TimeSeries a]
chunkTimeSeries f = groupBy ((==) `on` (f . snd))

timeSeriesTimes :: TimeSeries a -> [UTCTime]
timeSeriesTimes = map fst

getTime :: (UTCTime, a) -> UTCTime
getTime = fst

getValue :: (UTCTime, a) -> a
getValue = snd

type Ephemeris = TimeSeries EclipticPosition

type Chart = Map Planet EclipticPosition

data AspectType = Conjunction | Sextile | Square | Trine | Opposition
  deriving (Eq, Ord, Show)

class IsEvent e where
  startTime :: e -> UTCTime
  endTime :: e -> UTCTime
  summary :: e -> Text
  description :: e -> Maybe Text

data SignEvent = SignEvent
  { planet :: Planet,
    sign :: Maybe ZodiacSignName,
    signStartTime :: UTCTime,
    signEndTime :: UTCTime
  }

instance IsEvent SignEvent where
  startTime = signStartTime
  endTime = signEndTime
  summary e = pack [symbol (planet e), ' ', maybe '?' symbol (sign e)]
  description _ = Nothing

data AspectEvent = AspectEvent
  { aspect :: Aspect,
    aspectExactTime :: UTCTime,
    aspectStartTime :: UTCTime,
    aspectEndTime :: UTCTime
  }

instance IsEvent AspectEvent where
  startTime = aspectStartTime
  endTime = aspectEndTime
  summary (aspect -> a) = pack [symbol (planet1 a), ' ', symbol (aspectType a), ' ', symbol (planet2 a)]
  description e = Just $ pack ("Exact at " <> show (aspectExactTime e))

data RetrogradeEvent = RetrogradeEvent
  { retrogradePlanet :: Planet,
    retrogradeStartTime :: UTCTime,
    retrogradeEndTime :: UTCTime
  }

instance IsEvent RetrogradeEvent where
  startTime = retrogradeStartTime
  endTime = retrogradeEndTime
  summary e = pack [symbol (retrogradePlanet e), ' ', retrograde]
  description _ = Nothing

showLongitudeComponents :: LongitudeComponents -> String
showLongitudeComponents longitude
  | Just sign <- longitudeZodiacSign longitude,
    degrees <- longitudeDegrees longitude,
    minutes <- longitudeMinutes longitude =
      [symbol sign] ++ " " ++ show degrees ++ "° " ++ show minutes ++ "ʹ"
  | otherwise = "?"
