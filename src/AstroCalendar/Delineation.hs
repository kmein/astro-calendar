module AstroCalendar.Delineation (Delineations, getDelineations) where

import Data.Map (Map)
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Map qualified as Map
import AstroCalendar.Types (AspectType(..), parseAspectType, parsePlanet)
import SwissEphemeris (Planet(..))
import Data.Text.Lazy (Text)

type Delineations = Map (Planet, Planet, AspectType) Text

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


