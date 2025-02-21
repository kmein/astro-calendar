{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import AstroCalendar.Types (AspectType (..))
import AstroCalendar.ICalendar
import AstroCalendar.Ephemeris
import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Function (on)
import Data.List
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar (Year, fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import SwissEphemeris qualified as SwE
import Text.ICalendar.Printer
import Text.ICalendar.Types

main :: IO ()
main = do
  calendar <- astrologicalCalendar =<< ephemeris 2025
  BL.putStr $ printICalendar def calendar
