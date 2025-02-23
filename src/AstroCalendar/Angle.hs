{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Angle (Angle (..), difference, degreesMinutes) where

import Data.Aeson (ToJSON)
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
