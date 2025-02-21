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

allowedOrb :: Either Aspect Transit -> Angle
allowedOrb aspectOrTransit =
  let p1 = either planet1 natalPlanet aspectOrTransit
      p2 = either planet2 transitingPlanet aspectOrTransit
   in if p1 == SwE.Sun || p1 == SwE.Moon || p2 == SwE.Sun || p2 == SwE.Moon
        then 10
        else 8

isAllowed :: Either Aspect Transit -> Angle -> Bool
isAllowed aspectOrTransit distance =
  let allowableOrb = allowedOrb aspectOrTransit
      aspectDegrees = distanceDegrees (either aspectType transitType aspectOrTransit)
   in distance < aspectDegrees + allowableOrb && distance > aspectDegrees - allowableOrb

findAspects :: PlanetSelection -> Chart -> Map.Map Aspect Angle
findAspects planetSelection chart =
  Map.fromList $ mapMaybe getOrb (allAspects planetSelection)
  where
    getOrb aspect@(Aspect p1 p2 aspectType) =
      let l1 = Angle $ SwE.lng $ chart Map.! p1
          l2 = Angle $ SwE.lng $ chart Map.! p2
          diff = abs $ difference l1 l2
          deviation = diff - distanceDegrees aspectType
       in if isAllowed (Left aspect) diff
            then Just (aspect, abs deviation)
            else Nothing

findTransits :: PlanetSelection -> Chart -> Chart -> Map.Map Transit Angle
findTransits planetSelection natal chart = Map.fromList $ mapMaybe getOrb (allTransits planetSelection)
  where
    getOrb aspect@(Transit pn pt aspectType) =
      let ln = Angle $ SwE.lng $ natal Map.! pn
          lt = Angle $ SwE.lng $ chart Map.! pt
          diff = abs $ difference ln lt
          deviation = diff - distanceDegrees aspectType
       in if isAllowed (Right aspect) diff
            then
              Just
                ( Transit
                    { natalPlanet = pn,
                      transitingPlanet = pt,
                      transitType = aspectType
                    },
                  abs deviation
                )
            else Nothing
