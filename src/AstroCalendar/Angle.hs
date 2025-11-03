{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Angle (Angle (..), difference, degreesMinutes, midpoint, longitudeToEclipticPosition) where

import Data.Aeson (ToJSON)
import SwissEphemeris qualified as SwE
import Data.Fixed (mod')

newtype Angle = Angle {degrees :: Double}
  deriving newtype (Eq, Ord, Num, Real, Fractional, Show, ToJSON)

normalize :: Angle -> Angle
normalize angle =
  let angle' = angle `mod'` 360
   in if angle' < 0 then angle' + 360 else angle'

difference :: Angle -> Angle -> Angle
difference angle1 angle2 =
  let diff = normalize (angle1 - angle2)
   in if diff > 180 then diff - 360 else diff

degreesMinutes :: Angle -> (Int, Int)
degreesMinutes (degrees -> a) = (truncate a, round $ (a - fromInteger (truncate a)) * 60)

-- | Calculate the midpoint between two EclipticPosition values
midpoint :: SwE.EclipticPosition -> SwE.EclipticPosition -> SwE.EclipticPosition
midpoint pos1 pos2 =
  SwE.EclipticPosition
    { SwE.lng = midpointLongitude (SwE.lng pos1) (SwE.lng pos2),
      SwE.lat = avg (SwE.lat pos1) (SwE.lat pos2),
      SwE.distance = avg (SwE.distance pos1) (SwE.distance pos2),
      SwE.lngSpeed = avg (SwE.lngSpeed pos1) (SwE.lngSpeed pos2),
      SwE.latSpeed = avg (SwE.latSpeed pos1) (SwE.latSpeed pos2),
      SwE.distSpeed = avg (SwE.distSpeed pos1) (SwE.distSpeed pos2)
    }
  where
    avg a b = (a + b) / 2
    midpointLongitude lng1 lng2
      | abs (lng1 - lng2) <= 180 = avg lng1 lng2
      | lng1 < lng2 = avg (lng1 + 360) lng2 `mod'` 360
      | otherwise = avg lng1 (lng2 + 360) `mod'` 360

-- | Convert a longitude value into an EclipticPosition with default values
longitudeToEclipticPosition :: Double -> SwE.EclipticPosition
longitudeToEclipticPosition lng = SwE.EclipticPosition {SwE.lng = lng, SwE.lat = 0.0, SwE.distance = 0.0, SwE.lngSpeed = 0.0, SwE.latSpeed = 0.0, SwE.distSpeed = 0.0}

