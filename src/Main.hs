{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import AstroCalendar.Aspect
import AstroCalendar.Chart
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
                    <|> option
                      (CustomAspectTypes <$> parseAspectTypes)
                      ( long "only-aspects"
                          <> help "Use only the specified aspects"
                          <> metavar "ASPECT1,ASPECT2,..."
                      )
                    <|> pure AllAspectTypes
                )
            <*> ( flag' ChrisBrennan (long "orbs-brennan" <> help "Use 3° orbs")
                    <|> flag' LizGreene (long "orbs-greene" <> help "Use orbs like Liz Greene")
                    <|> flag' AstroDienst (long "orbs-astrodienst" <> help "Use orbs like astro.com (default)")
                    <|> flag' RichardTarnas (long "orbs-tarnas" <> help "Use 15° orbs")
                    <|> pure AstroDienst
                )
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
                    ( option
                        (parseUTCTime <|> parseDate)
                        ( long "date"
                            <> short 'd'
                            <> help "Date to chart (default: now)"
                            <> metavar "YYYY-MM-DD (HH:MM)"
                        )
                    )
              )
              (progDesc "Show chart for a date and aspects")
          )
          <> command
            "synastry"
            ( info
                ( Synastry
                    <$> optional
                      ( option
                          (parseUTCTime <|> parseDate)
                          ( long "left"
                              <> short 'l'
                              <> help "First date to chart"
                              <> metavar "YYYY-MM-DD (HH:MM)"
                          )
                      )
                    <*> optional
                      ( option
                          (parseUTCTime <|> parseDate)
                          ( long "right"
                              <> short 'r'
                              <> help "Second date to chart"
                              <> metavar "YYYY-MM-DD (HH:MM)"
                          )
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

parsePlanets :: ReadM [SwE.Planet]
parsePlanets = eitherReader (mapM parsePlanet . TL.splitOn "," . TL.pack)
  where
    parsePlanet = \case
      "sun" -> Right SwE.Sun
      "moon" -> Right SwE.Moon
      "mercury" -> Right SwE.Mercury
      "venus" -> Right SwE.Venus
      "mars" -> Right SwE.Mars
      "jupiter" -> Right SwE.Jupiter
      "saturn" -> Right SwE.Saturn
      "uranus" -> Right SwE.Uranus
      "neptune" -> Right SwE.Neptune
      "pluto" -> Right SwE.Pluto
      _ -> Left "Invalid planet"

parseAspectTypes :: ReadM [AspectType]
parseAspectTypes = eitherReader (mapM parseAspectType . TL.splitOn "," . TL.pack)
  where
    parseAspectType = \case
      "conjunction" -> Right Conjunction
      "opposition" -> Right Opposition
      "square" -> Right Square
      "sextile" -> Right Sextile
      "trine" -> Right Trine
      _ -> Left "Invalid aspect"

eventToString :: (IsEvent e) => EventsSettings -> e -> String
eventToString settings event =
  unwords
    [ strptime (startTime event),
      strptime (endTime event),
      TL.unpack (summary event),
      maybe "" strptime (maxTime event)
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
      chart <- natalChart options (fromMaybe now time)
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
