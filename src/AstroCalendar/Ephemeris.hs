module AstroCalendar.Ephemeris (natalChart) where

import AstroCalendar.Angle (longitudeToEclipticPosition, midpoint)
import AstroCalendar.Types
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe
import Data.Time.Clock (UTCTime (..))
import SwissEphemeris qualified as SwE

natalChart :: SelectionOptions -> UTCTime -> IO Chart
natalChart options utcTime = do
  maybeTime <- SwE.toJulianDay utcTime
  case maybeTime of
    Just time -> do
      angles <- case position options of
        Just position -> do
          cusps <- SwE.calculateCusps SwE.Placidus time position
          let angleCusps = SwE.angles cusps
          pure $
            Map.fromList
              [ (Ascendant, longitudeToEclipticPosition (SwE.ascendant angleCusps)),
                (MediumCaeli, longitudeToEclipticPosition (SwE.mc angleCusps))
              ]
        Nothing -> pure Map.empty
      planets <-
        Map.fromList . catMaybes
          <$> traverse
            ( \planet -> do
                position <- SwE.calculateEclipticPosition time planet
                pure $ (planet,) <$> eitherToMaybe position
            )
            (toList $ allPlanets options)
      let planetsAndAngles = Map.mapKeys Planet planets `Map.union` Map.mapKeys AnglePoint angles
          theMidpoints
            | EbertinPlanets <- planetSelection options =
                Map.fromList $ do
                  (p1, x1) <- Map.toList planetsAndAngles
                  (p2, x2) <- Map.toList planetsAndAngles
                  guard $ p1 < p2
                  pure (Midpoint p1 p2, midpoint x1 x2)
            | otherwise = Map.empty
      pure $ planetsAndAngles `Map.union` theMidpoints
    _ -> error $ "Could not convert to julian day: " ++ show utcTime
  where
    eitherToMaybe = either (const Nothing) Just
