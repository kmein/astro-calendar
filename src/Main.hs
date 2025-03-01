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
    <$> ( flag' ICS (long "ical" <> help "Write iCalendar")
            <|> flag' Text (long "text" <> help "Write plain text")
            <|> flag' JSON (long "json" <> help "Write JSON")
            <|> pure Text
        )
    <*> ( flag' TraditionalPlanets (long "traditional" <> help "Use traditional 7 planets")
            <|> flag' ModernPlanets (long "modern" <> help "Use modern 10 planets")
            <|> option
              (CustomPlanets <$> parsePlanets)
              ( long "only-planets"
                  <> short 'p'
                  <> help "Select only some planets"
                  <> metavar "PLANET1,PLANET2,..."
              )
            <|> pure TraditionalPlanets
        )
    <*> ( flag' AllAspectTypes (long "all-aspects" <> help "All aspects")
            <|> flag' HardAspectTypes (long "only-hard-aspects" <> help "Only hard aspects")
            <|> option
              (CustomAspectTypes <$> parseAspectTypes)
              ( long "only-aspects"
                  <> help "Select only some aspects"
                  <> metavar "ASPECT1,ASPECT2,..."
              )
            <|> pure AllAspectTypes
        )
    <*> switch
      ( long "interpret"
          <> short 'i'
          <> help "Whether to offer delineations on the chart"
      )
    <*> subparser
      ( command
          "chart"
          ( info
              ( Chart
                  <$> optional
                    ( option
                        parseUTCTime
                        ( long "date"
                            <> short 'd'
                            <> help "Date to chart"
                            <> metavar "YYYY-MM-DD HH:MM"
                        )
                    )
              )
              (progDesc "Show chart and aspects")
          )
          <> command
            "synastry"
            ( info
                ( Synastry
                    <$> optional
                      ( option
                          parseUTCTime
                          ( long "left"
                              <> short 'l'
                              <> help "First date to chart"
                              <> metavar "YYYY-MM-DD HH:MM"
                          )
                      )
                    <*> optional
                      ( option
                          parseUTCTime
                          ( long "right"
                              <> short 'r'
                              <> help "Second date to chart"
                              <> metavar "YYYY-MM-DD HH:MM"
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
                                  parseUTCTime
                                  ( long "begin"
                                      <> short 'b'
                                      <> help "Start date"
                                      <> metavar "YYYY-MM-DD HH:MM"
                                  )
                              )
                            <*> optional
                              ( option
                                  parseUTCTime
                                  ( long "end"
                                      <> short 'e'
                                      <> help "End date"
                                      <> metavar "YYYY-MM-DD HH:MM"
                                  )
                              )
                            <*> optional
                              ( option
                                  parseUTCTime
                                  ( long "transits"
                                      <> short 't'
                                      <> help "Birth date to calculate transits"
                                      <> metavar "YYYY-MM-DD HH:MM"
                                  )
                              )
                            <*> ( flag' Daily (long "daily" <> help "Check for every day")
                                    <|> flag' Hourly (long "hourly" <> help "Check for every hour")
                                    <|> flag' Monthly (long "monthly" <> help "Check for every month")
                                    <|> flag' Minutely (long "minutely" <> help "Check for every minute")
                                    <|> flag' Yearly (long "yearly" <> help "Check for every year")
                                    <|> pure Daily
                                )
                        )
                )
                (progDesc "Show events")
            )
      )

parseUTCTime :: ReadM UTCTime
parseUTCTime = eitherReader $ \input ->
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" input of
    Just time -> Right time
    Nothing -> Left "Invalid time format. Expected format: YYYY-MM-DD HH:MM"

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
eventToString settings event = unwords [strptime (startTime event), strptime (endTime event), TL.unpack (summary event), maybe "" TL.unpack (description event)]
  where
    strptime = formatTime defaultTimeLocale $ case settingsPrecision settings of
      Minutely -> "%Y-%m-%d %H:%M"
      Hourly -> "%Y-%m-%d %H"
      Daily -> "%Y-%m-%d"
      Monthly -> "%Y-%m"
      Yearly -> "%Y"

main :: IO ()
main = do
  settings <-
    execParser $
      info
        (sample <**> helper)
        (fullDesc <> progDesc "Print astrological events (transits, sign entries, retrogradations)")
  let planetSelection = settingsPlanets settings
      aspectSelection = settingsAspectTypes settings
  case astroCommand settings of
    Synastry {time1, time2} -> do
      now <- getCurrentTime
      chart1 <- natalChart planetSelection (fromMaybe now time1)
      chart2 <- natalChart planetSelection (fromMaybe now time2)
      let aspects = findTransits aspectSelection planetSelection chart1 chart2
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
      chart <- natalChart planetSelection (fromMaybe now time)
      let aspects = findAspects aspectSelection planetSelection chart
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
      events@(r, s, a, t, e) <- astrologicalEvents aspectSelection planetSelection eventsSettings =<< fullEphemeris planetSelection eventsSettings
      case settingsFormat settings of
        ICS -> do
          calendar <- astrologicalCalendar events
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
