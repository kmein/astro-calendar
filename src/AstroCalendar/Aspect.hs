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

allowedOrb :: SelectionOptions -> Aspect -> Angle
allowedOrb options aspectOrTransit =
  let p1 = planet1 aspectOrTransit
      p2 = planet2 aspectOrTransit
   in case orbSelection options of
        AstroDienst ->
          -- https://www.astro.com/astrology/in_aspect_e.htm
          let planets9 = [SwE.Sun, SwE.Moon, SwE.Jupiter, SwE.Saturn]
              planets5 = [SwE.Uranus, SwE.Neptune, SwE.Pluto]
              planets7 = [SwE.Mercury, SwE.Mars, SwE.Venus]
           in if aspectType aspectOrTransit == Sextile
                then 5
                else
                  if p1 `elem` planets9 || p2 `elem` planets9
                    then 9
                    else
                      if p1 `elem` planets7 || p2 `elem` planets7
                        then 7
                        else
                          if p1 `elem` planets5 || p2 `elem` planets5
                            then 5
                            else 0
        LizGreene ->
          -- https://www.astro.com/astrology/in_aspect_e.htm
          if aspectType aspectOrTransit `elem` [Conjunction, Opposition, Square, Trine]
            then 10
            else 6
        RichardTarnas -> 15
        ChrisBrennan ->
          -- https://theastrologypodcast.com/transcripts/ep-323-transcript-aspects-in-astrology-the-five-major-configurations/
          if p1 == SwE.Moon || p2 == SwE.Moon
            then 13
            else 3

isAllowed :: SelectionOptions -> Aspect -> Angle -> Bool
isAllowed options aspectOrTransit distance =
  let allowableOrb = allowedOrb options aspectOrTransit
      aspectDegrees = distanceDegrees (aspectType aspectOrTransit)
   in abs (distance - aspectDegrees) <= allowableOrb

findAspects :: SelectionOptions -> Chart -> Map.Map Aspect Angle
findAspects options chart =
  Map.fromList $ mapMaybe getOrb (allAspects options)
  where
    getOrb aspect@(Aspect p1 aspectType p2) =
      let l1 = Angle $ SwE.lng $ chart Map.! p1
          l2 = Angle $ SwE.lng $ chart Map.! p2
          diff = abs $ difference l1 l2
          deviation = abs $ diff - distanceDegrees aspectType
       in if isAllowed options aspect diff
            then Just (aspect, deviation)
            else Nothing

findTransits :: SelectionOptions -> Chart -> Chart -> Map.Map Aspect Angle
findTransits options natal chart = Map.fromList $ mapMaybe getOrb (allTransits options)
  where
    getOrb aspect@(Aspect pn aspectType pt) =
      let ln = Angle $ SwE.lng $ natal Map.! pn
          lt = Angle $ SwE.lng $ chart Map.! pt
          diff = abs $ difference ln lt
          deviation = abs $ diff - distanceDegrees aspectType
       in if isAllowed options aspect diff
            then
              Just
                ( Aspect pn aspectType pt,
                  deviation
                )
            else Nothing
