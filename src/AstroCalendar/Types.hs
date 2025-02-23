{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Types
  ( AspectType (..),
    AspectKind (..),
    Chart,
    Ephemeris,
    Aspect (..),
    TimeSeries,
    allAspects,
    allAspectTypes,
    allPlanets,
    allTransits,
    chunkTimeSeries,
    timeSeriesTimes,
    planetToJson,
    zodiacSignToJson,
    getTime,
    getValue,
    Symbol (..),
    AspectEvent (..),
    SignEvent (..),
    RetrogradeEvent (..),
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

instance ToJSON AspectType where
  toJSON =
    String . \case
      Conjunction -> "conjunction"
      Sextile -> "sextile"
      Square -> "square"
      Trine -> "trine"
      Opposition -> "opposition"

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

data Aspect = Aspect
  { aspectKind :: AspectKind,
    planet1 :: Planet,
    aspectType :: AspectType,
    planet2 :: Planet
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
  [ Aspect NatalAspect p1 t p2
    | p1 <- allPlanets planetSelection,
      p2 <- allPlanets planetSelection,
      p1 < p2,
      t <- allAspectTypes
  ]

allTransits :: PlanetSelection -> [Aspect]
allTransits planetSelection =
  [ Aspect TransitAspect pn t pt
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

data AspectKind = NatalAspect | TransitAspect
  deriving (Eq, Ord, Show)

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

planetToJson :: Planet -> Value
planetToJson = \case
  Mercury -> String "mercury"
  Venus -> String "venus"
  Mars -> String "mars"
  Jupiter -> String "jupiter"
  Saturn -> String "saturn"
  Uranus -> String "uranus"
  Neptune -> String "neptune"
  Pluto -> String "pluto"
  Moon -> String "moon"
  Sun -> String "sun"
  _ -> Null

zodiacSignToJson :: ZodiacSignName -> Value
zodiacSignToJson = \case
  Aries -> "aries"
  Taurus -> "taurus"
  Gemini -> "gemini"
  Cancer -> "cancer"
  Leo -> "leo"
  Virgo -> "virgo"
  Libra -> "libra"
  Scorpio -> "scorpio"
  Sagittarius -> "sagittarius"
  Capricorn -> "capricorn"
  Aquarius -> "aquarius"
  Pisces -> "pisces"

instance ToJSON SignEvent where
  toJSON event =
    object
      [ "planet" .= planetToJson (planet event),
        "startTime" .= startTime event,
        "endTime" .= endTime event,
        "sign" .= fmap zodiacSignToJson (sign event)
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
        planet1Name = case aspectKind (aspect event) of
          TransitAspect -> "natalPlanet"
          NatalAspect -> "planet1"
        planet2Name = case aspectKind (aspect event) of
          TransitAspect -> "transitingPlanet"
          NatalAspect -> "planet2"
     in object
          [ "startTime" .= startTime event,
            "endTime" .= endTime event,
            "exactTime" .= aspectExactTime event,
            planet1Name .= planetToJson (planet1 a),
            planet2Name .= planetToJson (planet2 a),
            "type" .= aspectType a
          ]

data RetrogradeEvent = RetrogradeEvent
  { retrogradePlanet :: Planet,
    retrogradeStartTime :: UTCTime,
    retrogradeEndTime :: UTCTime
  }

instance ToJSON RetrogradeEvent where
  toJSON event =
    object
      [ "startTime" .= startTime event,
        "endTime" .= endTime event,
        "planet" .= planetToJson (retrogradePlanet event)
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
  | Chart {time :: Maybe UTCTime}

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
