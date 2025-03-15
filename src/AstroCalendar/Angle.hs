{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Angle (Angle (..), difference, degreesMinutes, midpoint) where

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

radians :: Angle -> Double
radians x = degrees x * pi / 180

fromRadians :: Double -> Angle
fromRadians x = Angle (x * 180 / pi)

midpoint :: Angle -> Angle -> Angle
midpoint a b =
  let -- Convert longitudes to Cartesian coordinates
      (x1, y1) = (cos (radians a), sin (radians a))
      (x2, y2) = (cos (radians b), sin (radians b))

      -- Average Cartesian coordinates
      (xMid, yMid) = ((x1 + x2) / 2, (y1 + y2) / 2)

      -- Convert back to longitude (handling quadrant issues)
      midLng = mod' (fromRadians (atan2 yMid xMid)) 360
   in midLng
