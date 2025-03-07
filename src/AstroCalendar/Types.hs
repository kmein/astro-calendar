{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
    aspectString,
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
    SelectionOptions (..),
    Precision (..),
    formatTimeWithPrecision,
    EventsSettings (..),
    PlanetSelection (..),
    AspectTypeSelection (..),
    OrbSelection (..),
    EclipseEvent (..),
    retrograde,
    dateRange,
  )
where

import Data.Aeson
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import GHC.IO (unsafePerformIO)
import SwissEphemeris (EclipticPosition, FromJulianDay (..), JulianDayUT1, LunarEclipseInformation (..), Planet (..), SolarEclipseInformation (..), ZodiacSignName (..))

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

eclipse, occultation, retrograde :: Char
eclipse = '🝶'
occultation = '🝵'
retrograde = '℞'

data Aspect = Aspect
  { planet1 :: Planet,
    aspectType :: AspectType,
    planet2 :: Planet
  }
  deriving (Ord, Show)

aspectString :: Aspect -> String
aspectString aspect =
  [ symbol (planet1 aspect),
    ' ',
    symbol (aspectType aspect),
    ' ',
    symbol (planet2 aspect)
  ]

instance Eq Aspect where
  a1 == a2 =
    aspectType a1 == aspectType a2
      && ( (planet1 a1 == planet1 a2 && planet2 a1 == planet2 a2)
             || (planet1 a1 == planet2 a2 && planet2 a1 == planet1 a2)
         )

instance ToJSON Aspect where
  toJSON a =
    object
      [ ("planet1", planetToJson (planet1 a)),
        ("planet2", planetToJson (planet2 a)),
        ("type", toJSON (aspectType a))
      ]

allAspects :: SelectionOptions -> [Aspect]
allAspects options =
  [ Aspect p1 t p2
    | p1 <- allPlanets options,
      p2 <- allPlanets options,
      p1 < p2,
      t <- allAspectTypes options
  ]

allTransits :: SelectionOptions -> [Aspect]
allTransits options =
  [ Aspect pn t pt
    | pn <- allPlanets options,
      pt <- delete Moon (allPlanets options),
      t <- allAspectTypes options
  ]

allAspectTypes :: SelectionOptions -> [AspectType]
allAspectTypes (aspectTypeSelection -> AllAspectTypes) = sort [Conjunction, Sextile, Square, Trine, Opposition]
allAspectTypes (aspectTypeSelection -> HardAspectTypes) = sort [Conjunction, Square, Opposition]
allAspectTypes (aspectTypeSelection -> CustomAspectTypes cs) = sort cs

allPlanets :: SelectionOptions -> [Planet]
allPlanets (planetSelection -> ModernPlanets) = [Sun .. Pluto]
allPlanets (planetSelection -> TraditionalPlanets) = [Sun .. Saturn]
allPlanets (planetSelection -> CustomPlanets cs) = sort cs

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
  maxTime :: e -> Maybe UTCTime

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
  maxTime _ = Nothing

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

data AspectEvent (k :: AspectKind) = AspectEvent
  { aspect :: Aspect,
    aspectExactTime :: UTCTime,
    aspectStartTime :: UTCTime,
    aspectEndTime :: UTCTime
  }

instance IsEvent (AspectEvent NatalAspect) where
  startTime = aspectStartTime
  endTime = aspectEndTime
  summary (aspect -> a) = pack [symbol (planet1 a), ' ', symbol (aspectType a), ' ', symbol (planet2 a)]
  maxTime = Just . aspectExactTime

instance IsEvent (AspectEvent TransitAspect) where
  startTime = aspectStartTime
  endTime = aspectEndTime
  summary (aspect -> a) = pack [symbol (planet2 a), ' ', symbol (aspectType a)] <> " natal " <> pack [symbol (planet1 a)]
  maxTime = Just . aspectExactTime

instance ToJSON (AspectEvent NatalAspect) where
  toJSON event =
    let a = aspect event
     in object
          [ "startTime" .= startTime event,
            "endTime" .= endTime event,
            "exactTime" .= aspectExactTime event,
            "planet1" .= planetToJson (planet1 a),
            "planet2" .= planetToJson (planet2 a),
            "type" .= aspectType a
          ]

instance ToJSON (AspectEvent TransitAspect) where
  toJSON event =
    let a = aspect event
     in object
          [ "startTime" .= startTime event,
            "endTime" .= endTime event,
            "exactTime" .= aspectExactTime event,
            "natalPlanet" .= planetToJson (planet1 a),
            "transitingPlanet" .= planetToJson (planet2 a),
            "type" .= aspectType a
          ]

data EclipseEvent = SolarEclipse SolarEclipseInformation | LunarEclipse LunarEclipseInformation

instance IsEvent EclipseEvent where
  startTime =
    unsafeJulianToUTC . \case
      SolarEclipse e -> solarEclipseBegin e
      LunarEclipse e -> lunarEclipsePartialPhaseBegin e
  endTime =
    unsafeJulianToUTC . \case
      SolarEclipse e -> solarEclipseEnd e
      LunarEclipse e -> lunarEclipsePartialPhaseEnd e
  summary = \case
    SolarEclipse e -> pack $ [occultation, ' ', symbol Sun, ' '] ++ show (solarEclipseType e)
    LunarEclipse e -> pack $ [eclipse, ' ', symbol Moon, ' '] ++ show (lunarEclipseType e)
  maxTime =
    Just . unsafeJulianToUTC . \case
      LunarEclipse e -> lunarEclipseMax e
      SolarEclipse e -> solarEclipseMax e

unsafeJulianToUTC :: JulianDayUT1 -> UTCTime
unsafeJulianToUTC = unsafePerformIO . fromJulianDay

instance ToJSON EclipseEvent where
  toJSON event =
    object
      [ "startTime" .= startTime event,
        "endTime" .= endTime event,
        "kind" .= case event of
          LunarEclipse _ -> "lunar" :: String
          SolarEclipse _ -> "solar"
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
  maxTime _ = Nothing

data Format = ICS | Text | JSON

data Precision = Yearly | Monthly | Daily | Hourly | Minutely
  deriving (Show)

formatTimeWithPrecision :: (FormatTime a) => Precision -> a -> String
formatTimeWithPrecision precision = formatTime defaultTimeLocale $ case precision of
  Minutely -> "%Y-%m-%d %H:%M"
  Hourly -> "%Y-%m-%d %H"
  Daily -> "%Y-%m-%d"
  Monthly -> "%Y-%m"
  Yearly -> "%Y"

data PlanetSelection
  = TraditionalPlanets
  | ModernPlanets
  | CustomPlanets [Planet]

data AspectTypeSelection
  = AllAspectTypes
  | HardAspectTypes
  | CustomAspectTypes [AspectType]

data OrbSelection
  = ChrisBrennan
  | RichardTarnas
  | AstroDienst
  | LizGreene

data Command
  = Events EventsSettings
  | Chart {time :: Maybe UTCTime}
  | Synastry {time1 :: Maybe UTCTime, time2 :: Maybe UTCTime}
  | Commonalities {times :: [UTCTime]}

data EventsSettings = EventsSettings
  { withRetrograde :: Bool,
    withAspects :: Bool,
    withSigns :: Bool,
    withEclipses :: Bool,
    settingsBegin :: Maybe UTCTime,
    settingsEnd :: Maybe UTCTime,
    transitsTo :: Maybe UTCTime,
    settingsPrecision :: Precision
  }
  deriving (Show)

data SelectionOptions = SelectionOptions
  { planetSelection :: PlanetSelection,
    aspectTypeSelection :: AspectTypeSelection,
    orbSelection :: OrbSelection
  }

data Settings = Settings
  { settingsFormat :: Format,
    settingsSelectionOptions :: SelectionOptions,
    settingsInterpret :: Bool,
    astroCommand :: Command
  }

currentYear :: Year
currentYear = 2025

dateRange :: EventsSettings -> (UTCTime, UTCTime)
dateRange settings = (beginning, end)
  where
    beginning = fromMaybe (UTCTime (fromGregorian currentYear 1 1) 0) (settingsBegin settings)
    end = fromMaybe (UTCTime (fromGregorian currentYear 12 31) 86400) (settingsEnd settings)
