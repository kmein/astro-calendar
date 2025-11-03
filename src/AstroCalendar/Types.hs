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
    AnglePoint (..),
    Ephemeris,
    Aspect (..),
    TimeSeries,
    EventStringSettings (..),
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
    Point (..),
    Command (..),
    Format (..),
    SelectionOptions (..),
    Precision (..),
    formatTimeWithPrecision,
    parsePlanet,
    parseAspectType,
    EventsSettings (..),
    PlanetSelection (..),
    AspectTypeSelection (..),
    OrbSelection (..),
    EclipseEvent (..),
    retrograde,
    dateRange,
    toConfiguredZonedTime,
  )
where

import Data.Aeson
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import GHC.IO (unsafePerformIO)
import SwissEphemeris (EclipticPosition, FromJulianDay (..), GeographicPosition (..), JulianDayUT1, LunarEclipseInformation (..), Planet (..), SolarEclipseInformation (..), ZodiacSignName (..))
import Data.Time.Zones (TZ, timeZoneForUTCTime)
import Data.Time.LocalTime (ZonedTime, utcToZonedTime, utc)

data EventStringSettings = WithDate | WithoutDate

data Point
  = Midpoint Point Point
  | Planet Planet
  | AnglePoint AnglePoint
  deriving (Show, Ord, Eq)

data AnglePoint = Ascendant | MediumCaeli
  deriving (Show, Ord, Eq)

instance ToJSON AnglePoint where
  toJSON = \case
    Ascendant -> "ascendant"
    MediumCaeli -> "medium-caeli"

instance Symbol AnglePoint where
  symbol = \case
    Ascendant -> "A"
    MediumCaeli -> "M"

instance ToJSON Point where
  toJSON = \case
    Planet p -> planetToJson p
    AnglePoint a -> toJSON a
    Midpoint a b -> object ["midpoint" .= [toJSON a, toJSON b]]

instance Symbol Point where
  symbol = \case
    Planet p -> symbol p
    AnglePoint a -> symbol a
    Midpoint x y -> symbol x ++ "/" ++ symbol y

class Symbol a where
  symbol :: a -> String

instance Symbol AspectType where
  symbol = \case
    Conjunction -> "☌"
    Sesquiquadrate -> "⚼"
    Sextile -> "⚹"
    Square -> "□"
    SemiSquare -> "∠"
    Trine -> "△"
    Opposition -> "☍"

instance ToJSON AspectType where
  toJSON =
    String . \case
      Conjunction -> "conjunction"
      Sextile -> "sextile"
      Square -> "square"
      Sesquiquadrate -> "sesquiquadrate"
      SemiSquare -> "semisquare"
      Trine -> "trine"
      Opposition -> "opposition"

instance Symbol ZodiacSignName where
  symbol = \case
    Aries -> "♈"
    Taurus -> "♉"
    Gemini -> "♊"
    Cancer -> "♋"
    Leo -> "♌"
    Virgo -> "♍"
    Libra -> "♎"
    Scorpio -> "♏"
    Sagittarius -> "♐"
    Capricorn -> "♑"
    Aquarius -> "♒"
    Pisces -> "♓"

instance Symbol Planet where
  symbol = \case
    Mercury -> "☿"
    Venus -> "♀"
    Mars -> "♂"
    Jupiter -> "♃"
    Saturn -> "♄"
    Uranus -> "♅"
    Neptune -> "♆"
    Pluto -> "♇"
    Moon -> "☽"
    Sun -> "☉"
    TrueNode -> "☊"
    _ -> "\xfffd" -- replacement character

parsePlanet :: Text -> Either String Planet
parsePlanet = \case
  "sun" -> Right Sun
  "moon" -> Right Moon
  "mercury" -> Right Mercury
  "venus" -> Right Venus
  "mars" -> Right Mars
  "jupiter" -> Right Jupiter
  "saturn" -> Right Saturn
  "uranus" -> Right Uranus
  "neptune" -> Right Neptune
  "pluto" -> Right Pluto
  _ -> Left "Invalid planet"

eclipse, occultation, retrograde :: String
eclipse = "🝶"
occultation = "🝵"
retrograde = "℞"

data Aspect = Aspect
  { point1 :: Point,
    aspectType :: AspectType,
    point2 :: Point
  }
  deriving (Ord, Show)

aspectString :: Aspect -> String
aspectString aspect =
  unwords
    [ symbol (point1 aspect),
      symbol (aspectType aspect),
      symbol (point2 aspect)
    ]

parseAspectType :: Text -> Either String AspectType
parseAspectType = \case
  "conjunction" -> Right Conjunction
  "opposition" -> Right Opposition
  "square" -> Right Square
  "sextile" -> Right Sextile
  "trine" -> Right Trine
  _ -> Left "Invalid aspect"

instance Eq Aspect where
  a1 == a2 =
    aspectType a1 == aspectType a2
      && ( (point1 a1 == point1 a2 && point2 a1 == point2 a2)
             || (point1 a1 == point2 a2 && point2 a1 == point1 a2)
         )

instance ToJSON Aspect where
  toJSON a =
    object
      [ ("points", toJSON [point1 a, point2 a]),
        ("type", toJSON (aspectType a))
      ]

allAspects :: SelectionOptions -> [Aspect]
allAspects options =
  [ Aspect (Planet p1) t (Planet p2)
    | p1 <- allPlanets options,
      p2 <- allPlanets options,
      p1 < p2,
      t <- allAspectTypes options
  ]

allTransits :: SelectionOptions -> [Aspect]
allTransits options =
  [ Aspect (Planet pn) t (Planet pt)
    | pn <- allPlanets options,
      pt <- delete Moon (allPlanets options),
      t <- allAspectTypes options
  ]

allAspectTypes :: SelectionOptions -> [AspectType]
allAspectTypes (aspectTypeSelection -> AllAspectTypes) = sort [Conjunction, Sextile, Square, Trine, Opposition]
allAspectTypes (aspectTypeSelection -> HardAspectTypes) = sort [Conjunction, Square, Opposition]
allAspectTypes (aspectTypeSelection -> CustomAspectTypes cs) = sort cs
allAspectTypes (aspectTypeSelection -> EbertinAspectTypes) = sort [Conjunction, Square, SemiSquare, Sesquiquadrate, Opposition]

allPlanets :: SelectionOptions -> [Planet]
allPlanets (planetSelection -> ModernPlanets) = [Sun .. Pluto]
allPlanets (planetSelection -> TraditionalPlanets) = [Sun .. Saturn]
allPlanets (planetSelection -> CustomPlanets cs) = sort cs
allPlanets (planetSelection -> EbertinPlanets) = [Sun .. Pluto] ++ [TrueNode]

type TimeSeries a = Map UTCTime a

chunkTimeSeries :: (Eq b) => (a -> b) -> TimeSeries a -> [TimeSeries a]
chunkTimeSeries f = map Map.fromList . groupBy ((==) `on` (f . snd)) . Map.toAscList

timeSeriesTimes :: TimeSeries a -> [UTCTime]
timeSeriesTimes = sort . Map.keys

getTime :: (UTCTime, a) -> UTCTime
getTime = fst

getValue :: (UTCTime, a) -> a
getValue = snd

type Ephemeris = TimeSeries EclipticPosition

type Chart = Map Point EclipticPosition

data AspectKind = NatalAspect | TransitAspect
  deriving (Eq, Ord, Show)

data AspectType = Conjunction | Sextile | Square | Trine | Opposition | Sesquiquadrate | SemiSquare
  deriving (Eq, Ord, Show)

class IsEvent e where
  startTime :: e -> UTCTime
  endTime :: e -> UTCTime
  summary :: e -> Text
  maxTime :: e -> Maybe UTCTime

data SignEvent = SignEvent
  { planet :: Point,
    sign :: Maybe ZodiacSignName,
    signStartTime :: UTCTime,
    signEndTime :: UTCTime
  }

instance IsEvent SignEvent where
  startTime = signStartTime
  endTime = signEndTime
  summary e = pack $ unwords [symbol (planet e), maybe "?" symbol (sign e)]
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
  TrueNode -> String "lunarNode"
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
      [ "planet" .= planet event,
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
  summary (aspect -> a) = pack $ unwords [symbol (point1 a), symbol (aspectType a), symbol (point2 a)]
  maxTime = Just . aspectExactTime

instance IsEvent (AspectEvent TransitAspect) where
  startTime = aspectStartTime
  endTime = aspectEndTime
  summary (aspect -> a) = pack (unwords [symbol (point2 a), symbol (aspectType a)]) <> " natal " <> pack (symbol (point1 a))
  maxTime = Just . aspectExactTime

instance ToJSON (AspectEvent NatalAspect) where
  toJSON event =
    let a = aspect event
     in object
          [ "startTime" .= startTime event,
            "endTime" .= endTime event,
            "exactTime" .= aspectExactTime event,
            "points" .= [point1 a, point2 a],
            "type" .= aspectType a
          ]

instance ToJSON (AspectEvent TransitAspect) where
  toJSON event =
    let a = aspect event
     in object
          [ "startTime" .= startTime event,
            "endTime" .= endTime event,
            "exactTime" .= aspectExactTime event,
            "natalPoint" .= point1 a,
            "transitingPoint" .= point2 a,
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
    SolarEclipse e -> pack $ unwords [occultation, symbol Sun, show (solarEclipseType e)]
    LunarEclipse e -> pack $ unwords [eclipse, symbol Moon, show (lunarEclipseType e)]
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
  { retrogradePlanet :: Point,
    retrogradeStartTime :: UTCTime,
    retrogradeEndTime :: UTCTime
  }

instance ToJSON RetrogradeEvent where
  toJSON event =
    object
      [ "startTime" .= startTime event,
        "endTime" .= endTime event,
        "planet" .= retrogradePlanet event
      ]

instance IsEvent RetrogradeEvent where
  startTime = retrogradeStartTime
  endTime = retrogradeEndTime
  summary e = pack $ unwords [symbol (retrogradePlanet e), retrograde]
  maxTime _ = Nothing

data Format = ICS | Text | JSON

data Precision = Yearly | Monthly | Daily | Hourly | Minutely
  deriving (Show)

formatTimeWithPrecision :: (FormatTime a) => Precision -> a -> String
formatTimeWithPrecision precision = formatTime defaultTimeLocale $ case precision of
  Minutely -> "%Y-%m-%d %H:%M %Z"
  Hourly -> "%Y-%m-%d %H %Z"
  Daily -> "%Y-%m-%d"
  Monthly -> "%Y-%m"
  Yearly -> "%Y"

data PlanetSelection
  = TraditionalPlanets
  | ModernPlanets
  | CustomPlanets [Planet]
  | EbertinPlanets

data AspectTypeSelection
  = AllAspectTypes
  | HardAspectTypes
  | CustomAspectTypes [AspectType]
  | EbertinAspectTypes

data OrbSelection
  = ChrisBrennan
  | RichardTarnas
  | AstroDienst
  | LizGreene
  | ReinholdEbertin

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
    orbSelection :: OrbSelection,
    position :: Maybe GeographicPosition,
    tzSelection :: Maybe TZ
  }

data Settings = Settings
  { settingsFormat :: Format,
    settingsSelectionOptions :: SelectionOptions,
    settingsInterpret :: Bool,
    astroCommand :: Command
  }

currentYear :: Year
{-# NOINLINE currentYear #-}
currentYear = unsafePerformIO $ fmap (\t -> let (y, _, _) = toGregorian (utctDay t) in y) getCurrentTime

toConfiguredZonedTime :: SelectionOptions -> UTCTime -> ZonedTime
toConfiguredZonedTime options utcTime =
  let maybeConfiguredTimeZone = (`timeZoneForUTCTime` utcTime) <$> tzSelection options
      configuredTimeZone = fromMaybe utc maybeConfiguredTimeZone
   in utcToZonedTime configuredTimeZone utcTime

dateRange :: EventsSettings -> (UTCTime, UTCTime)
dateRange settings = (beginning, end)
  where
    beginning = fromMaybe (UTCTime (fromGregorian currentYear 1 1) 0) (settingsBegin settings)
    end = fromMaybe (UTCTime (fromGregorian currentYear 12 31) 86400) (settingsEnd settings)
