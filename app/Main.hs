{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AstroCalendar.Aspect
import AstroCalendar.Chart
import AstroCalendar.Commonalities
import AstroCalendar.Ephemeris
import AstroCalendar.Event
import AstroCalendar.ICalendar
import AstroCalendar.Interpretation
import AstroCalendar.Types
import Control.Monad
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Default
import Data.List (sort)
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import Safe
import SwissEphemeris qualified as SwE
import Text.ICalendar.Printer

sample :: Parser Settings
sample =
  Settings
    <$> ( flag' ICS (long "ical" <> help "Output iCalendar")
            <|> flag' Text (long "text" <> help "Output plain text")
            <|> flag' JSON (long "json" <> help "Output JSON")
            <|> pure Text
        )
    <*> ( SelectionOptions
            <$> ( flag' TraditionalPlanets (long "traditional" <> help "Use traditional 7 planets")
                    <|> flag' ModernPlanets (long "modern" <> help "Use modern 10 planets (default)")
                    <|> flag' EbertinPlanets (long "ebertin-planets" <> help "Use Ebertin planets (including midpoints)")
                    <|> option
                      (CustomPlanets <$> parsePlanets)
                      ( long "only-planets"
                          <> short 'p'
                          <> help "Select only the specified planets"
                          <> metavar "PLANET1,PLANET2,..."
                      )
                    <|> pure ModernPlanets
                )
            <*> ( flag' AllAspectTypes (long "all-aspects" <> help "Use all aspects (default)")
                    <|> flag' HardAspectTypes (long "only-hard-aspects" <> help "Use only hard aspects")
                    <|> flag' EbertinAspectTypes (long "ebertin-aspects" <> help "Use only Ebertin aspects")
                    <|> option
                      (CustomAspectTypes <$> parseAspectTypes)
                      ( long "only-aspects"
                          <> help "Use only the specified aspects"
                          <> metavar "ASPECT1,ASPECT2,..."
                      )
                    <|> pure AllAspectTypes
                )
            <*> ( flag' ChrisBrennan (long "brennan-orbs" <> help "Use 3° orbs")
                    <|> flag' LizGreene (long "greene-orbs" <> help "Use orbs like Liz Greene")
                    <|> flag' AstroDienst (long "astrodienst-orbs" <> help "Use orbs like astro.com (default)")
                    <|> flag' RichardTarnas (long "tarnas-orbs" <> help "Use 15° orbs")
                    <|> flag' ReinholdEbertin (long "ebertin-orbs" <> help "Use orbs like Reinhold Ebertin")
                    <|> pure AstroDienst
                )
            <*> optional (option parseGeographicPosition (long "at" <> help "Calculate houses for certain location"))
        )
    <*> switch
      ( long "interpret"
          <> short 'i'
          <> help "Offer GPT-generated delineations on the chart"
      )
    <*> subparser
      ( command
          "chart"
          ( info
              ( Chart
                  <$> optional
                    ( argument
                        (parseUTCTime <|> parseDate)
                        (help "Date to chart (default: now)" <> metavar "YYYY-MM-DD (HH:MM)")
                    )
              )
              (progDesc "Show chart for a date and aspects")
          )
          <> command
            "commonalities"
            ( info
                ( Commonalities
                    <$> some (argument (parseUTCTime <|> parseDate) (metavar "YYYY-MM-DD (HH:MM)"))
                )
                $ progDesc "Analyse astrological commonalities of multiple dates"
            )
          <> command
            "synastry"
            ( info
                ( Synastry
                    <$> optional
                      ( argument
                          (parseUTCTime <|> parseDate)
                          (help "First date to chart" <> metavar "YYYY-MM-DD (HH:MM)")
                      )
                    <*> optional
                      ( argument
                          (parseUTCTime <|> parseDate)
                          (help "Second date to chart" <> metavar "YYYY-MM-DD (HH:MM)")
                      )
                )
                (progDesc "Show two chart and the aspects between them")
            )
          <> command
            "events"
            ( info
                ( Events
                    <$> ( EventsSettings
                            <$> switch
                              ( long "retrograde"
                                  <> short 'r'
                                  <> help "Include retrograde periods"
                              )
                            <*> switch
                              ( long "aspects"
                                  <> short 'a'
                                  <> help "Include aspects"
                              )
                            <*> switch
                              ( long "signs"
                                  <> short 's'
                                  <> help "Include sign transitions"
                              )
                            <*> switch
                              ( long "eclipses"
                                  <> help "Include eclipses"
                              )
                            <*> optional
                              ( option
                                  (parseUTCTime <|> parseDate)
                                  ( long "begin"
                                      <> short 'b'
                                      <> help "Start date"
                                      <> metavar "YYYY-MM-DD (HH:MM)"
                                  )
                              )
                            <*> optional
                              ( option
                                  (parseUTCTime <|> parseDate)
                                  ( long "end"
                                      <> short 'e'
                                      <> help "End date"
                                      <> metavar "YYYY-MM-DD (HH:MM)"
                                  )
                              )
                            <*> optional
                              ( option
                                  (parseUTCTime <|> parseDate)
                                  ( long "transits"
                                      <> short 't'
                                      <> help "Birth date to calculate transits"
                                      <> metavar "YYYY-MM-DD (HH:MM)"
                                  )
                              )
                            <*> ( flag' Daily (long "daily" <> help "Check every day (default)")
                                    <|> flag' Hourly (long "hourly" <> help "Check every hour")
                                    <|> flag' Monthly (long "monthly" <> help "Check every month")
                                    <|> flag' Minutely (long "minutely" <> help "Check every minute")
                                    <|> flag' Yearly (long "yearly" <> help "Check every year")
                                    <|> pure Daily
                                )
                        )
                )
                (progDesc "Show astrological alignments over time")
            )
      )

parseUTCTime :: ReadM UTCTime
parseUTCTime = eitherReader $ \input ->
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" input of
    Just time -> Right time
    Nothing -> Left "Invalid time format. Expected format: YYYY-MM-DD HH:MM"

parseDate :: ReadM UTCTime
parseDate = eitherReader $ \input ->
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" input of
    Just time -> Right time
    Nothing -> Left "Invalid time format. Expected format: YYYY-MM-DD (HH:MM)"

parseGeographicPosition :: ReadM SwE.GeographicPosition
parseGeographicPosition = eitherReader $ \input ->
  case span (/= ',') input of
    (latString, ',' : lonString)
      | Just lat <- readMay @Double latString,
        Just lon <- readMay @Double lonString ->
          Right $ SwE.GeographicPosition lat lon
    _ -> Left "Invalid position format: Expected format: LAT,LON"

parsePlanets :: ReadM [SwE.Planet]
parsePlanets = eitherReader (mapM parsePlanet . TL.splitOn "," . TL.pack)

parseAspectTypes :: ReadM [AspectType]
parseAspectTypes = eitherReader (mapM parseAspectType . TL.splitOn "," . TL.pack)

eventToString :: (IsEvent e) => EventsSettings -> e -> String
eventToString settings event =
  unwords
    [ strptime (startTime event),
      strptime (endTime event),
      TL.unpack (summary event),
      "TODO"
    ]
  where
    strptime = formatTimeWithPrecision (settingsPrecision settings)

main :: IO ()
main = do
  settings <-
    execParser $
      info
        (sample <**> helper)
        (fullDesc <> progDesc "Command-line astrology toolkit")
  let options = settingsSelectionOptions settings
  case astroCommand settings of
    Commonalities times -> do
      charts <- mapM (natalChart options) times
      let commonalities = chartCommonalities options charts
      case settingsFormat settings of
        JSON -> BL.putStrLn $ JSON.encode $ commonalitiesJson commonalities
        Text -> do
          putStrLn $ commonalitiesString commonalities
          when (settingsInterpret settings) $ do
            let c = BL.unpack $ JSON.encode $ commonalitiesJson commonalities
            delineations <- sendRequest $ "The following astrological alignments are common to several world events. Please explain the archetypes that unite them in one paragraph each (one for placements, one for retrogrades, and one for aspects).\n\n" ++ c
            maybe (return ()) putStrLn delineations
        ICS -> error "ICS format is not supported for commonalities."
    Synastry {time1, time2} -> do
      now <- getCurrentTime
      chart1 <- natalChart options (fromMaybe now time1)
      chart2 <- natalChart options (fromMaybe now time2)
      let aspects = findTransits options chart1 chart2
      case settingsFormat settings of
        JSON -> BL.putStrLn $ JSON.encode $ chartJson [chart1, chart2] aspects
        Text -> do
          putStrLn $ chartString [chart1, chart2] aspects
          when (settingsInterpret settings) $
            let c = BL.unpack $ JSON.encode $ chartJson [chart1, chart2] aspects
             in case (time1, time2) of
                  (Just _, Just _) -> do
                    delineations <- sendRequest $ "Please concisely interpret the following synastry chart in three paragraphs (one for chart 1, one for chart 2, one for the aspects):\n\n" ++ c
                    maybe (return ()) putStrLn delineations
                  (Just _, Nothing) -> do
                    delineations <- sendRequest $ "Please concisely characterize the current transit situation for the birth chart (the 1st one) in one paragraph:\n\n" ++ c
                    maybe (return ()) putStrLn delineations
                  (Nothing, Just _) -> do
                    delineations <- sendRequest $ "Please concisely characterize the current transit situation for the birth chart (the 2nd one) in one paragraph:\n\n" ++ c
                    maybe (return ()) putStrLn delineations
                  _ -> fail "No date given."
        ICS -> error "ICS format is not supported for synastry."
    Chart {time} -> do
      now <- getCurrentTime
      let time' = fromMaybe now time
      chart <- natalChart options time'
      let aspects = findAspects options chart
      case settingsFormat settings of
        JSON -> BL.putStrLn $ JSON.encode $ chartJson [chart] aspects
        Text -> do
          putStrLn $ chartString [chart] aspects
          when (settingsInterpret settings) $ do
            let c = BL.unpack $ JSON.encode $ chartJson [chart] aspects
            delineations <- sendRequest $ "Please concisely interpret the following birth chart in two paragraphs (one for the placements, one for the aspects):\n\n" ++ c
            maybe (return ()) putStrLn delineations
        ICS -> error "ICS format is not supported for charts."
    Events eventsSettings -> do
      events@(r, s, a, t, e) <- astrologicalEvents options eventsSettings
      case settingsFormat settings of
        ICS -> do
          calendar <- astrologicalCalendar (settingsPrecision eventsSettings) events
          BL.putStrLn $ printICalendar def calendar
        Text ->
          mapM_ putStrLn $
            sort $
              concat
                [ maybe [] (map (eventToString eventsSettings)) r,
                  maybe [] (map (eventToString eventsSettings)) s,
                  maybe [] (map (eventToString eventsSettings)) a,
                  maybe [] (map (eventToString eventsSettings)) t,
                  maybe [] (map (eventToString eventsSettings)) e
                ]
        JSON -> do
          BL.putStrLn $
            JSON.encode $
              JSON.object
                [ "retrograde" JSON..= JSON.toJSON r,
                  "sign" JSON..= JSON.toJSON s,
                  "aspect" JSON..= JSON.toJSON a,
                  "transits" JSON..= JSON.toJSON t,
                  "eclipses" JSON..= JSON.toJSON e
                ]
