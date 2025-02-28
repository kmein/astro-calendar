module AstroCalendar.Eclipse (findEclipses) where

import AstroCalendar.Types
import SwissEphemeris qualified as SwE

findEclipses :: EventsSettings -> IO [EclipseEvent]
findEclipses settings =
  (++)
    <$> (map SolarEclipse <$> findSolarEclipses settings)
    <*> (map LunarEclipse <$> findLunarEclipses settings)

findAllBetween ::
  (SwE.JulianDayUT1 -> IO (Either String eclipse)) ->
  (eclipse -> SwE.JulianDayUT1) ->
  (SwE.JulianDayUT1, SwE.JulianDayUT1) ->
  [eclipse] ->
  IO [eclipse]
findAllBetween findNextFrom getEndDate (start, end) acc = do
  eitherNext <- findNextFrom start
  case eitherNext of
    Right next ->
      if getEndDate next > end
        then return acc
        else findAllBetween findNextFrom getEndDate (getEndDate next, end) (next : acc)
    Left s -> fail s

findLunarEclipses :: EventsSettings -> IO [SwE.LunarEclipseInformation]
findLunarEclipses settings = do
  let (beginning, end) = dateRange settings
  Just beginning' <- SwE.toJulianDay beginning
  Just end' <- SwE.toJulianDay end
  findAllBetween findNext SwE.lunarEclipseTotalityEnd (beginning', end') []
  where
    findNext :: SwE.JulianDayUT1 -> IO (Either String SwE.LunarEclipseInformation)
    findNext = SwE.nextLunarEclipse [] SwE.SearchForward

findSolarEclipses :: EventsSettings -> IO [SwE.SolarEclipseInformation]
findSolarEclipses settings = do
  let (beginning, end) = dateRange settings
  Just beginning' <- SwE.toJulianDay beginning
  Just end' <- SwE.toJulianDay end
  findAllBetween findNext SwE.solarEclipseEnd (beginning', end') []
  where
    findNext :: SwE.JulianDayUT1 -> IO (Either String SwE.SolarEclipseInformation)
    findNext = SwE.nextSolarEclipse [] SwE.SearchForward
