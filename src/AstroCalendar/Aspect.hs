{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Aspect (Aspect, findAspects) where

import AstroCalendar.Angle
import AstroCalendar.Types
import Data.Map qualified as Map
import Data.Maybe
import SwissEphemeris qualified as SwE

distanceDegrees :: AspectType -> Angle
distanceDegrees =
  \case
    Conjunction -> 0
    Sextile -> 60
    Square -> 90
    Trine -> 120
    Opposition -> 180

allowedOrb :: Aspect -> Angle
allowedOrb Aspect {planet1, planet2, aspectType} =
  if planet1 == SwE.Sun || planet1 == SwE.Moon || planet2 == SwE.Sun || planet2 == SwE.Moon
    then 10
    else 8

isAllowed :: Aspect -> Angle -> Bool
isAllowed aspect distance =
  let allowableOrb = allowedOrb aspect
      aspectDegrees = distanceDegrees (aspectType aspect)
   in distance < aspectDegrees + allowableOrb && distance > aspectDegrees - allowableOrb

findAspects :: Maybe Chart -> Chart -> Map.Map Aspect Angle
findAspects natal chart = Map.fromList $ mapMaybe getOrb $
  case natal of
    Just _ -> [Aspect p1 p2 t | p1 <- allPlanets, p2 <- allPlanets, t <- allAspectTypes]
    Nothing -> allAspects
  where
    getOrb aspect@(Aspect p1 p2 aspectType) =
      let l1 = Angle $ SwE.lng $ fromMaybe chart natal Map.! p1
          l2 = Angle $ SwE.lng $ chart Map.! p2
          diff = abs $ difference l1 l2
          deviation = diff - distanceDegrees aspectType
       in if isAllowed aspect diff
            then Just (aspect, abs deviation)
            else Nothing
