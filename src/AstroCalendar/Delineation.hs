{-# LANGUAGE LambdaCase #-}
module AstroCalendar.Delineation (Delineations, getDelineations) where

import AstroCalendar.Types (AspectType (..), parseAspectType, parsePlanet)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Vector qualified as V
import SwissEphemeris (Planet (..))
import System.Directory (doesFileExist)
import System.Environment
import System.IO (hPutStrLn, stderr)

type Delineations = (Planet, Planet, AspectType) -> Maybe Text

getDelineations :: IO Delineations
getDelineations = do
  delineationsPath <- fromMaybe "delineations.csv" <$> lookupEnv "DELINEATIONS_CSV"
  pathExists <- doesFileExist delineationsPath
  if pathExists
    then do
      csvData <- BL.readFile delineationsPath
      vec <- either fail return $ Csv.decode Csv.NoHeader csvData
      let mp = Map.fromList $ do
            (x, y, z, a) <- V.toList vec
            case (,,) <$> parsePlanet x <*> parsePlanet y <*> parseAspectType z of
              Right (p1, p2, aspectType) ->
                let delineation = a :: Text
                 in return ((min p1 p2, max p1 p2, aspectType), delineation)
              Left err -> error $ "Error parsing CSV: " ++ err
      return $ flip Map.lookup mp
    else do
      hPutStrLn stderr $ "Warning: Delineations file not found at " ++ delineationsPath ++ ", proceeding without delineations."
      return $ const Nothing
