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
    <*> ( flag' ICS (long "ICS" <> help "Write iCalendar")
            <|> flag' Text (long "text" <> help "Write plain text")
            <|> flag' JSON (long "json" <> help "Write JSON")
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
        )

parseUTCTime :: ReadM UTCTime
parseUTCTime = eitherReader $ \input ->
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" input of
    Just time -> Right time
    Nothing -> Left "Invalid time format. Expected format: YYYY-MM-DD HH:MM"

eventToString :: (IsEvent e) => Settings -> e -> String
eventToString settings event = unwords [strptime (startTime event), strptime (endTime event), TL.unpack (summary event)]
  where
    strptime = formatTime defaultTimeLocale $ case settingsAccuracy settings of
      Minutely -> "%Y-%m-%d %H:%M"
      Hourly -> "%Y-%m-%d %H"
      Daily -> "%Y-%m-%d"
      Monthly -> "%Y-%m"

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
      maybe (pure ()) (mapM_ (putStrLn . eventToString settings)) r
      maybe (pure ()) (mapM_ (putStrLn . eventToString settings)) s
      maybe (pure ()) (mapM_ (putStrLn . eventToString settings)) a
      maybe (pure ()) (mapM_ (putStrLn . eventToString settings)) t
    JSON -> do
      BL.putStr $
        JSON.encode $
          JSON.object
            [ "retrograde" JSON..= JSON.toJSON r,
              "sign" JSON..= JSON.toJSON s,
              "aspect" JSON..= JSON.toJSON a,
              "transits" JSON..= JSON.toJSON t
            ]
