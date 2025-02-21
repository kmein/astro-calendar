{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Types
  ( AspectType (..),
    Chart,
    Ephemeris,
    Aspect (..),
    Transit (..),
    TimeSeries,
    allAspects,
    allAspectTypes,
    allPlanets,
    allTransits,
    chunkTimeSeries,
    timeSeriesTimes,
    getTime,
    getValue,
    Symbol (..),
    AspectEvent (..),
    SignEvent (..),
    RetrogradeEvent (..),
    TransitEvent (..),
    IsEvent (..),
    Settings (..),
    Command (..),
    Format (..),
    Accuracy (..),
    EventsSettings (..),
    PlanetSelection (..),
  )
where

import Data.Aeson
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Text qualified as T (pack)
import Data.Text.Lazy (Text, pack)
import Data.Time.Clock
import SwissEphemeris (EclipticPosition, Planet (..), ZodiacSignName (..))

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
    Pluto -> '♇'
    Moon -> '☽'
    Sun -> '☉'
    _ -> '\xfffd' -- replacement character

retrograde :: Char
retrograde = '℞'

data Transit = Transit
  { natalPlanet :: Planet,
    transitingPlanet :: Planet,
    transitType :: AspectType
  }
  deriving (Eq, Ord, Show)

data Aspect = Aspect
  { planet1 :: Planet,
    planet2 :: Planet,
    aspectType :: AspectType
  }
  deriving (Ord, Show)

instance Eq Aspect where
  a1 == a2 =
    aspectType a1 == aspectType a2
      && ( (planet1 a1 == planet1 a2 && planet2 a1 == planet2 a2)
             || (planet1 a1 == planet2 a2 && planet2 a1 == planet1 a2)
         )

allAspects :: PlanetSelection -> [Aspect]
allAspects planetSelection =
  [ Aspect p1 p2 t
    | p1 <- allPlanets planetSelection,
      p2 <- allPlanets planetSelection,
      p1 < p2,
      t <- allAspectTypes
  ]

allTransits :: PlanetSelection -> [Transit]
allTransits planetSelection =
  [ Transit pn pt t
    | pn <- allPlanets planetSelection,
      pt <- delete Moon (allPlanets planetSelection),
      t <- allAspectTypes
  ]

allAspectTypes :: [AspectType]
allAspectTypes = [Conjunction, Sextile, Square, Trine, Opposition]

allPlanets :: PlanetSelection -> [Planet]
allPlanets Modern = [Sun .. Pluto]
allPlanets Traditional = [Sun .. Saturn]

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

symbolToJson :: (Symbol a) => a -> Value
symbolToJson p = String (T.pack [symbol p])

instance ToJSON SignEvent where
  toJSON event =
    object
      [ "planet" .= symbolToJson (planet event),
        "startTime" .= toJSON (startTime event),
        "endTime" .= toJSON (endTime event),
        "sign" .= fmap symbolToJson (sign event)
      ]

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
  description e = Just $ pack ("closest at " <> show (aspectExactTime e))

instance ToJSON AspectEvent where
  toJSON event =
    let a = aspect event
     in object
          [ "startTime" .= toJSON (startTime event),
            "endTime" .= toJSON (endTime event),
            "exactTime" .= toJSON (aspectExactTime event),
            "planet1" .= symbolToJson (planet1 a),
            "planet2" .= symbolToJson (planet2 a),
            "type" .= symbolToJson (aspectType a)
          ]

data TransitEvent = TransitEvent
  { transit :: Transit,
    transitExactTime :: UTCTime,
    transitStartTime :: UTCTime,
    transitEndTime :: UTCTime
  }

instance IsEvent TransitEvent where
  startTime = transitStartTime
  endTime = transitEndTime
  summary (transit -> t) = pack [symbol (transitingPlanet t), ' ', symbol (transitType t)] <> " natal " <> pack [symbol (natalPlanet t)]
  description e = Just $ pack ("closest at " <> show (transitExactTime e))

instance ToJSON TransitEvent where
  toJSON event =
    let t = transit event
     in object
          [ "startTime" .= toJSON (startTime event),
            "endTime" .= toJSON (endTime event),
            "exactTime" .= toJSON (transitExactTime event),
            "planet1" .= symbolToJson (natalPlanet t),
            "planet2" .= symbolToJson (transitingPlanet t),
            "type" .= symbolToJson (transitType t)
          ]

data RetrogradeEvent = RetrogradeEvent
  { retrogradePlanet :: Planet,
    retrogradeStartTime :: UTCTime,
    retrogradeEndTime :: UTCTime
  }

instance ToJSON RetrogradeEvent where
  toJSON event =
    object
      [ "startTime" .= toJSON (startTime event),
        "endTime" .= toJSON (endTime event),
        "planet" .= symbolToJson (retrogradePlanet event)
      ]

instance IsEvent RetrogradeEvent where
  startTime = retrogradeStartTime
  endTime = retrogradeEndTime
  summary e = pack [symbol (retrogradePlanet e), ' ', retrograde]
  description _ = Nothing

data Format = ICS | Text | JSON

data Accuracy = Monthly | Daily | Hourly | Minutely

data PlanetSelection = Traditional | Modern

data Command
  = Events EventsSettings
  | Chart {birthTime :: UTCTime}

data EventsSettings = EventsSettings
  { withRetrograde :: Bool,
    withAspects :: Bool,
    withSigns :: Bool,
    settingsBegin :: Maybe UTCTime,
    settingsEnd :: Maybe UTCTime,
    transitsTo :: Maybe UTCTime,
    settingsAccuracy :: Accuracy
  }

data Settings = Settings
  { settingsFormat :: Format,
    settingsPlanets :: PlanetSelection,
    astroCommand :: Command
  }
