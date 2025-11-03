{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module AstroCalendar.Aspect (findAspects, findTransits) where

import AstroCalendar.Angle
import AstroCalendar.Types
import Control.Monad
import Data.Map qualified as Map
import SwissEphemeris qualified as SwE

distanceDegrees :: AspectType -> Angle
distanceDegrees =
  \case
    Conjunction -> 0
    Sextile -> 60
    Square -> 90
    Trine -> 120
    Opposition -> 180
    SemiSquare -> 45
    Sesquiquadrate -> 135

allowedOrb :: SelectionOptions -> Aspect -> Angle
allowedOrb options aspectOrTransit =
  let p1 = point1 aspectOrTransit
      p2 = point2 aspectOrTransit
   in case orbSelection options of
        ReinholdEbertin
          | Midpoint _ _ <- p1 -> 1.5
          | Midpoint _ _ <- p2 -> 1.5
          | let planets = map Planet [SwE.Sun, SwE.Moon],
            p1 `elem` planets || p2 `elem` planets ->
              5
          | let planets = map Planet [SwE.Mercury, SwE.Venus, SwE.Mars],
            p1 `elem` planets || p2 `elem` planets ->
              4
          | let planets = map Planet [SwE.Jupiter, SwE.Saturn, SwE.Uranus, SwE.Neptune, SwE.Pluto, SwE.TrueNode],
            p1 `elem` planets || p2 `elem` planets ->
              3
          | otherwise -> 0
        AstroDienst
          -- https://www.astro.com/astrology/in_aspect_e.htm
          | aspectType aspectOrTransit == Sextile -> 5
          | let planets9 = map Planet [SwE.Sun, SwE.Moon, SwE.Jupiter, SwE.Saturn],
            p1 `elem` planets9 || p2 `elem` planets9 ->
              9
          | let planets7 = map Planet [SwE.Mercury, SwE.Mars, SwE.Venus],
            p1 `elem` planets7 || p2 `elem` planets7 ->
              7
          | let planets5 = map Planet [SwE.Uranus, SwE.Neptune, SwE.Pluto],
            p1 `elem` planets5 || p2 `elem` planets5 ->
              5
          | otherwise -> 0
        LizGreene
          -- https://www.astro.com/astrology/in_aspect_e.htm
          | aspectType aspectOrTransit `elem` [Conjunction, Opposition, Square, Trine] -> 10
          | otherwise -> 6
        RichardTarnas ->
          case aspectType aspectOrTransit of
            Opposition -> 15
            Conjunction -> 15
            Square -> 10
            _ -> 10 -- ?
        ChrisBrennan
          -- https://theastrologypodcast.com/transcripts/ep-323-transcript-aspects-in-astrology-the-five-major-configurations/
          | p1 == Planet SwE.Moon || p2 == Planet SwE.Moon -> 13
          | otherwise -> 3

findAspects :: SelectionOptions -> Chart -> Map.Map Aspect Angle
findAspects options chart = findTransits options chart chart

findTransits :: SelectionOptions -> Chart -> Chart -> Map.Map Aspect Angle
findTransits options (Map.toList -> chartA) (Map.toList -> chartB) =
  Map.fromList $ do
    (p1, x1) <- chartA
    (p2, x2) <- chartB
    when (chartA == chartB) $ guard $ p1 < p2
    aspectType <- allAspectTypes options
    guard $ case (p1, aspectType, p2) of -- only look at conjunctions between midpoints
      -- (Midpoint _ _, Conjunction, Midpoint _ _) -> True
      (Midpoint _ _, _, Midpoint _ _) -> False
      _ -> True
    let diff = abs $ Angle (SwE.getEclipticLongitude x1) `difference` Angle (SwE.getEclipticLongitude x2)
        orb = abs $ diff - distanceDegrees aspectType
        aspect = Aspect p1 aspectType p2
    guard $ orb <= allowedOrb options aspect
    pure (aspect, orb)
