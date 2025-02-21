{-# LANGUAGE OverloadedStrings #-}

import AstroCalendar.Ephemeris
import AstroCalendar.Event
import AstroCalendar.ICalendar
import AstroCalendar.Types
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Text.Lazy qualified as TL
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import Text.ICalendar.Printer

sample :: Parser Settings
sample =
  Settings
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
    <*> option
      auto
      ( long "year"
          <> help "For which year to generate the calendar"
          <> showDefault
          <> value 2025
          <> metavar "YEAR"
      )
    <*> ( flag' ICS (long "ICS" <> help "Write iCalendar")
            <|> flag' Text (long "text" <> help "Write plain text")
            <|> flag' JSON (long "json" <> help "Write JSON")
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
    <*> option
      auto
      ( long "accuracy"
          <> help "How many minutes to skip between data points"
          <> showDefault
          <> value 60
          <> metavar "MINUTES"
      )

parseUTCTime :: ReadM UTCTime
parseUTCTime = eitherReader $ \input ->
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" input of
    Just time -> Right time
    Nothing -> Left "Invalid time format. Expected format: YYYY-MM-DD HH:MM"

eventToString :: (IsEvent e) => e -> String
eventToString event = unwords [strptime (startTime event), strptime (endTime event), TL.unpack (summary event)]
  where
    strptime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

main :: IO ()
main = do
  settings <-
    execParser $
      info
        (sample <**> helper)
        (fullDesc <> progDesc "Print astrological events (transits, sign entries, retrogradations)")
  events@(r, s, a, t) <- astrologicalEvents settings =<< fullEphemeris settings
  case settingsFormat settings of
    ICS -> do
      calendar <- astrologicalCalendar events
      BL.putStr $ printICalendar def calendar
    Text -> do
      maybe (pure ()) (mapM_ (putStrLn . eventToString)) r
      maybe (pure ()) (mapM_ (putStrLn . eventToString)) s
      maybe (pure ()) (mapM_ (putStrLn . eventToString)) a
      maybe (pure ()) (mapM_ (putStrLn . eventToString)) t
    JSON -> do
      BL.putStr $
        JSON.encode $
          JSON.object
            [ "retrograde" JSON..= JSON.toJSON r,
              "sign" JSON..= JSON.toJSON s,
              "aspect" JSON..= JSON.toJSON a,
              "transits" JSON..= JSON.toJSON t
            ]
