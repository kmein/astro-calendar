{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
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
    aspectString,
    allAspects,
    allAspectTypes,
    allPlanets,
    allTransits,
    aspectTypeFromName,
    chunkTimeSeries,
    timeSeriesTimes,
    planetToJson,
    zodiacSignToJson,
    startDate,
    endDate,
    aspectTypeToJson,
    getTime,
    getValue,
    transitString,
    eventString,
    eventJson,
    Symbol (..),
    Settings (..),
    Point (..),
    Command (..),
    Format (..),
    SelectionOptions (..),
    parsePlanet,
    parseAspectType,
    EventsSettings (..),
    PlanetSelection (..),
    AspectTypeSelection (..),
    OrbSelection (..),
    retrograde,
    dateRange,
  )
where

import Almanac qualified
import Almanac.Extras qualified as Almanac
import Control.Monad (guard)
import Data.Aeson
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Time.Calendar
import Data.Time.Clock
import SwissEphemeris (EclipticPosition, GeographicPosition (..), Planet (..), ZodiacSignName (..), dayFromJulianDay)

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
    Midpoint a b ->
      let planets = [toJSON a, toJSON b] :: [Value]
       in object ["midpoint" .= planets]

instance Symbol Point where
  symbol = \case
    Planet p -> symbol p
    AnglePoint a -> symbol a
    Midpoint x y -> symbol x ++ "/" ++ symbol y

class Symbol a where
  symbol :: a -> String

instance Symbol Almanac.Zodiac where
  symbol = symbol . Almanac.signName

instance Symbol Almanac.AspectName where
  symbol = \case
    Almanac.Conjunction -> "☌"
    Almanac.Sesquisquare -> "⚼"
    Almanac.Sextile -> "⚹"
    Almanac.Square -> "□"
    Almanac.SemiSquare -> "∠"
    Almanac.Trine -> "△"
    Almanac.Opposition -> "☍"
    Almanac.Quincunx -> "⚻"
    Almanac.SemiSextile -> "⚺"
    Almanac.Novile -> "⅑"
    Almanac.Quintile -> "⅕"
    Almanac.BiQuintile -> "⅖"
    Almanac.Septile -> "⅐"

aspectTypeToJson :: Almanac.AspectName -> Value
aspectTypeToJson =
  String . \case
    Almanac.Conjunction -> "conjunction"
    Almanac.Sextile -> "sextile"
    Almanac.Square -> "square"
    Almanac.Sesquisquare -> "sesquiquadrate"
    Almanac.SemiSquare -> "semisquare"
    Almanac.Trine -> "trine"
    Almanac.Opposition -> "opposition"
    Almanac.Quincunx -> "quincunx"
    Almanac.SemiSextile -> "semisextile"
    Almanac.Novile -> "novile"
    Almanac.Quintile -> "quintile"
    Almanac.BiQuintile -> "biquintile"
    Almanac.Septile -> "septile"

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
  deriving (Show, Ord)

aspectString :: Aspect -> String
aspectString aspect =
  unwords
    [ symbol (point1 aspect),
      symbol (Almanac.aspectName $ getAspectType $ aspectType aspect),
      symbol (point2 aspect)
    ]

transitString :: (Symbol a) => AspectKind -> Almanac.Transit a -> String
transitString natalOrMundane (Almanac.Transit {Almanac.transiting, Almanac.transited, Almanac.aspect, Almanac.transitStarts, Almanac.transitEnds}) =
  let t0 = dayFromJulianDay transitStarts
      t1 = dayFromJulianDay transitEnds
   in unwords
        [ show t0,
          show t1,
          symbol transiting,
          symbol aspect,
          case natalOrMundane of
            Natal -> "natal " <> symbol transited
            Mundane -> symbol transited
        ]

crossingString :: (Symbol a) => Almanac.Crossing a -> String
crossingString (Almanac.Crossing {Almanac.crossingStarts, Almanac.crossingEnds, Almanac.crossingPlanet, Almanac.crossingCrosses}) =
  let t0 = dayFromJulianDay crossingStarts
      t1 = dayFromJulianDay crossingEnds
   in unwords [show t0, show t1, symbol crossingPlanet, symbol crossingCrosses]

stationString :: Almanac.PlanetStation -> String
stationString (Almanac.PlanetStation {Almanac.stationStarts, Almanac.stationEnds, Almanac.stationPlanet, Almanac.stationType}) =
  let t0 = dayFromJulianDay stationStarts
      t1 = dayFromJulianDay stationEnds
      stationTypeString = case stationType of
        Almanac.StationaryRetrograde -> "SR"
        Almanac.StationaryDirect -> "SD"
        Almanac.Retrograde -> "R"
        Almanac.Direct -> "D"
   in unwords [show t0, show t1, symbol stationPlanet, stationTypeString]

eclipseDate :: Almanac.EclipseInfo -> Day
eclipseDate = \case
  Almanac.SolarEclipse _ day -> dayFromJulianDay day
  Almanac.LunarEclipse _ day -> dayFromJulianDay day

eclipseString :: Almanac.EclipseInfo -> String
eclipseString = \case
  Almanac.SolarEclipse solarEclipseType day -> unwords [show (dayFromJulianDay day), occultation, symbol Sun, show solarEclipseType]
  Almanac.LunarEclipse lunarEclipseType day -> unwords [show (dayFromJulianDay day), eclipse, symbol Moon, show lunarEclipseType]

eventJson :: (AspectKind, Almanac.Event) -> Value
eventJson = \case
  (natalOrMundane, Almanac.PlanetaryTransit transit) -> transitJson natalOrMundane transit
  (_, Almanac.Eclipse eclipseInfo) -> eclipseJson eclipseInfo
  (_, Almanac.ZodiacIngress crossing) -> crossingJson crossing
  (_, Almanac.DirectionChange station) -> stationJson station
  _ -> "not implemented"

stationJson :: Almanac.PlanetStation -> Value
stationJson (Almanac.PlanetStation {Almanac.stationStarts, Almanac.stationEnds, Almanac.stationPlanet, Almanac.stationType}) =
  object
    [ "start" .= dayFromJulianDay stationStarts,
      "end" .= dayFromJulianDay stationEnds,
      "planet" .= planetToJson stationPlanet,
      "type" .= case stationType of
        Almanac.Retrograde -> String "retrograde"
        _ -> error "station type not implemented"
    ]

crossingJson :: Almanac.Crossing Almanac.Zodiac -> Value
crossingJson (Almanac.Crossing {Almanac.crossingStarts, Almanac.crossingEnds, Almanac.crossingPlanet, Almanac.crossingCrosses}) =
  object
    [ "start" .= dayFromJulianDay crossingStarts,
      "end" .= dayFromJulianDay crossingEnds,
      "planet" .= planetToJson crossingPlanet,
      "sign" .= zodiacSignToJson (Almanac.signName crossingCrosses),
      "type" .= String "sign"
    ]

eclipseJson :: Almanac.EclipseInfo -> Value
eclipseJson eclipseInfo =
  object
    [ "start" .= eclipseDate eclipseInfo,
      "planet"
        .= String
          ( case eclipseInfo of
              Almanac.LunarEclipse _ _ -> "moon"
              Almanac.SolarEclipse _ _ -> "sun"
          ),
      "type" .= String "eclipse"
    ]

transitJson :: AspectKind -> Almanac.Transit Planet -> Value
transitJson natalOrMundane (Almanac.Transit {Almanac.transiting, Almanac.transited, Almanac.aspect, Almanac.transitStarts, Almanac.transitEnds}) =
  object
    [ "start" .= dayFromJulianDay transitStarts,
      "end" .= dayFromJulianDay transitEnds,
      "aspect" .= aspectTypeToJson aspect,
      "planet" .= planetToJson transiting,
      "planet2" .= planetToJson transited,
      "type"
        .= String
          ( case natalOrMundane of
              Natal -> "natal"
              Mundane -> "mundane"
          )
    ]

startDate :: Almanac.Event -> Day
startDate = \case
  Almanac.PlanetaryTransit t -> dayFromJulianDay $ Almanac.transitStarts t
  Almanac.DirectionChange c -> dayFromJulianDay $ Almanac.stationStarts c
  Almanac.ZodiacIngress c -> dayFromJulianDay $ Almanac.crossingStarts c
  Almanac.Eclipse e -> eclipseDate e
  s -> error $ "startDate not implemented for " ++ show s

endDate :: Almanac.Event -> Day
endDate = \case
  Almanac.PlanetaryTransit t -> dayFromJulianDay $ Almanac.transitEnds t
  Almanac.DirectionChange c -> dayFromJulianDay $ Almanac.stationEnds c
  Almanac.ZodiacIngress c -> dayFromJulianDay $ Almanac.crossingEnds c
  Almanac.Eclipse e -> eclipseDate e
  s -> error $ "endDate not implemented for " ++ show s

eventString :: (AspectKind, Almanac.Event) -> String
eventString = \case
  (natalOrMundane, Almanac.PlanetaryTransit transit) -> transitString natalOrMundane transit
  (_, Almanac.Eclipse eclipseInfo) -> eclipseString eclipseInfo
  (_, Almanac.ZodiacIngress crossing) -> crossingString crossing
  (_, Almanac.DirectionChange station) -> stationString station
  _ -> "not implemented"

aspectTypeFromName :: Almanac.AspectName -> AspectType
aspectTypeFromName =
  AspectType . \case
    Almanac.Opposition -> Almanac.opposition
    Almanac.Conjunction -> Almanac.conjunction
    Almanac.Sextile -> Almanac.sextile
    Almanac.Square -> Almanac.square
    Almanac.Trine -> Almanac.trine
    Almanac.Quincunx -> Almanac.quincunx
    Almanac.SemiSextile -> Almanac.semiSextile
    Almanac.Quintile -> Almanac.quintile
    Almanac.BiQuintile -> Almanac.biQuintile
    Almanac.SemiSquare -> Almanac.semiSquare
    Almanac.Sesquisquare -> Almanac.sesquisquare
    s -> error $ "no aspect defined for " ++ show s

parseAspectType :: Text -> Either String AspectType
parseAspectType =
  fmap AspectType . \case
    "conjunction" -> Right Almanac.conjunction
    "opposition" -> Right Almanac.opposition
    "square" -> Right Almanac.square
    "sextile" -> Right Almanac.sextile
    "trine" -> Right Almanac.trine
    _ -> Left "Invalid aspect"

instance Eq Aspect where
  a1 == a2 =
    aspectType a1 == aspectType a2
      && ( (point1 a1 == point1 a2 && point2 a1 == point2 a2)
             || (point1 a1 == point2 a2 && point2 a1 == point1 a2)
         )

instance ToJSON Aspect where
  toJSON a =
    let planets = [point1 a, point2 a] :: [Point]
     in object
          [ ("points", toJSON planets),
            ("type", aspectTypeToJson $ Almanac.aspectName $ getAspectType $ aspectType a)
          ]

allAspects :: SelectionOptions -> NonEmpty Aspect
allAspects options = NonEmpty.fromList $ do
  p1 <- toList $ allPlanets options
  p2 <- toList $ allPlanets options
  guard $ p1 < p2
  t <- toList $ allAspectTypes options
  pure $ Aspect (Planet p1) (AspectType t) (Planet p2)

allTransits :: SelectionOptions -> NonEmpty Aspect
allTransits options = do
  pn <- allPlanets options
  pt <- allPlanets options
  t <- allAspectTypes options
  pure $ Aspect (Planet pn) (AspectType t) (Planet pt)

allAspectTypes :: SelectionOptions -> NonEmpty Almanac.Aspect
allAspectTypes (aspectTypeSelection -> AllAspectTypes) = [Almanac.conjunction, Almanac.sextile, Almanac.square, Almanac.trine, Almanac.opposition]
allAspectTypes (aspectTypeSelection -> HardAspectTypes) = [Almanac.conjunction, Almanac.square, Almanac.opposition]
allAspectTypes (aspectTypeSelection -> CustomAspectTypes cs) = fmap getAspectType cs
allAspectTypes (aspectTypeSelection -> EbertinAspectTypes) = [Almanac.conjunction, Almanac.square, Almanac.semiSquare, Almanac.sesquisquare, Almanac.opposition]

allPlanets :: SelectionOptions -> NonEmpty Planet
allPlanets (planetSelection -> ModernPlanets) = [Sun .. Pluto]
allPlanets (planetSelection -> TraditionalPlanets) = [Sun .. Saturn]
allPlanets (planetSelection -> CustomPlanets cs) = NonEmpty.sort cs
allPlanets (planetSelection -> EbertinPlanets) = [Sun .. Pluto] <> [TrueNode]

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

data AspectKind = Natal | Mundane
  deriving (Eq, Ord, Show)

newtype AspectType = AspectType {getAspectType :: Almanac.Aspect}
  deriving (Eq, Show)

instance Ord AspectType where
  compare = compare `on` (Almanac.angle . getAspectType)

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

data Format = ICS | Text | JSON

data PlanetSelection
  = TraditionalPlanets
  | ModernPlanets
  | CustomPlanets (NonEmpty Planet)
  | EbertinPlanets

data AspectTypeSelection
  = AllAspectTypes
  | HardAspectTypes
  | CustomAspectTypes (NonEmpty AspectType)
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
    transitsTo :: Maybe UTCTime
  }
  deriving (Show)

data SelectionOptions = SelectionOptions
  { planetSelection :: PlanetSelection,
    aspectTypeSelection :: AspectTypeSelection,
    orbSelection :: OrbSelection,
    position :: Maybe GeographicPosition
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
