import AstroCalendar.ICalendar
import AstroCalendar.Ephemeris
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Text.ICalendar.Printer

main :: IO ()
main = do
  calendar <- astrologicalCalendar =<< fullEphemeris 2025
  BL.putStr $ printICalendar def calendar
