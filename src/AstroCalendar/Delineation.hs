{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Delineation (Delineations, getDelineations, getDelineationFor) where

import Almanac qualified
import AstroCalendar.Types (AspectKind (..), AspectType (..), aspectTypeFromName, parseAspectType, parsePlanet)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.Lazy (Text)
import Data.Vector qualified as V
import SwissEphemeris (Planet (..))

type Delineations = Map (Planet, Planet, AspectType) Text

getDelineationFor :: Delineations -> (AspectKind, Almanac.Event) -> Maybe Text
getDelineationFor delineations = \case
  (Mundane, Almanac.PlanetaryTransit (Almanac.Transit {Almanac.transiting, Almanac.transited, Almanac.aspect})) ->
    Map.lookup
      ( min transiting transited,
        max transiting transited,
        aspectTypeFromName aspect
      )
      delineations
  _ -> Nothing

getDelineations :: IO Delineations
getDelineations = do
  csvData <- BL.readFile "delineations.csv"
  vec <- either fail return $ Csv.decode Csv.NoHeader csvData
  let mp = Map.fromList $ do
        (x, y, z, a) <- V.toList vec
        case (,,) <$> parsePlanet x <*> parsePlanet y <*> parseAspectType z of
          Right (p1, p2, aspectType) ->
            let delineation = a :: Text
             in return ((min p1 p2, max p1 p2, aspectType), delineation)
          Left err -> error $ "Error parsing CSV: " ++ err
  return mp
