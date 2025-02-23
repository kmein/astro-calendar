{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Aspect (findAspects, findTransits) where

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
allowedOrb aspectOrTransit =
  let p1 = planet1 aspectOrTransit
      p2 = planet2 aspectOrTransit
   in if p1 == SwE.Sun || p1 == SwE.Moon || p2 == SwE.Sun || p2 == SwE.Moon
        then 10
        else 8

isAllowed :: Aspect -> Angle -> Bool
isAllowed aspectOrTransit distance =
  let allowableOrb = allowedOrb aspectOrTransit
      aspectDegrees = distanceDegrees (aspectType aspectOrTransit)
   in abs (distance - aspectDegrees) <= allowableOrb

findAspects :: PlanetSelection -> Chart -> Map.Map Aspect Angle
findAspects planetSelection chart =
  Map.fromList $ mapMaybe getOrb (allAspects planetSelection)
  where
    getOrb aspect@(Aspect _ p1 aspectType p2) =
      let l1 = Angle $ SwE.lng $ chart Map.! p1
          l2 = Angle $ SwE.lng $ chart Map.! p2
          diff = abs $ difference l1 l2
          deviation = abs $ diff - distanceDegrees aspectType
       in if isAllowed aspect diff
            then Just (aspect, deviation)
            else Nothing

findTransits :: PlanetSelection -> Chart -> Chart -> Map.Map Aspect Angle
findTransits planetSelection natal chart = Map.fromList $ mapMaybe getOrb (allTransits planetSelection)
  where
    getOrb aspect@(Aspect _ pn aspectType pt) =
      let ln = Angle $ SwE.lng $ natal Map.! pn
          lt = Angle $ SwE.lng $ chart Map.! pt
          diff = abs $ difference ln lt
          deviation = abs $ diff - distanceDegrees aspectType
       in if isAllowed aspect diff
            then
              Just
                ( Aspect
                    { planet1 = pn,
                      planet2 = pt,
                      aspectType = aspectType,
                      aspectKind = TransitAspect
                    },
                  deviation
                )
            else Nothing
