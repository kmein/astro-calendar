{-# LANGUAGE DerivingStrategies #-}

module AstroCalendar.Angle (Angle (..), difference) where

import Data.Fixed (mod')

newtype Angle = Angle {degrees :: Double}
  deriving newtype (Eq, Ord, Num, Real, Show)

normalize :: Angle -> Angle
normalize angle =
  let angle' = angle `mod'` 360
   in if angle' < 0 then angle' + 360 else angle'

difference :: Angle -> Angle -> Angle
difference angle1 angle2 =
  let diff = normalize (angle1 - angle2)
   in if diff > 180 then diff - 360 else diff
