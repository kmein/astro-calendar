{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.ICalendar where

import Almanac qualified
import AstroCalendar.Delineation (Delineations, getDelineationFor, getDelineations)
import AstroCalendar.Event
import AstroCalendar.Types
import Data.Default
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.UUID.V4 qualified as UUID
import Text.ICalendar

eventVEvent :: Delineations -> AspectKind -> Almanac.Event -> IO VEvent
eventVEvent delineations natalOrMundane event = do
  uuid <- TL.fromStrict . UUID.toText <$> UUID.nextRandom
  let description = getDelineationFor delineations (natalOrMundane, event)
      summary = getSummary (natalOrMundane, event)
  pure $
    VEvent
      { veDescription =
          fmap
            ( \d ->
                ( Description
                    { descriptionValue = d,
                      descriptionAltRep = def,
                      descriptionLanguage = def,
                      descriptionOther = def
                    }
                )
            )
            description,
        veSummary =
          Just
            ( Summary
                { summaryValue = summary,
                  summaryOther = def,
                  summaryAltRep = def,
                  summaryLanguage = def
                }
            ),
        veDTStamp =
          DTStamp
            { dtStampValue = UTCTime (fromGregorian 1970 1 1) 0.0,
              dtStampOther = def
            },
        veUID = UID {uidValue = uuid, uidOther = def},
        veDTStart =
          Just
            DTStartDate
              { dtStartDateValue = Date (startDate event),
                dtStartOther = def
              },
        veDTEndDuration =
          Just $
            Left
              DTEndDate
                { dtEndDateValue = Date (endDate event),
                  dtEndOther = def
                },
        veTransp = Transparent {timeTransparencyOther = def},
        veClass = def,
        veCreated = def,
        veGeo = def,
        veLastMod = def,
        veLocation = def,
        veOrganizer = def,
        vePriority = def,
        veSeq = def,
        veStatus = def,
        veUrl = def,
        veRecurId = def,
        veRRule = def,
        veAttach = def,
        veAttendee = def,
        veCategories = def,
        veComment = def,
        veContact = def,
        veExDate = def,
        veRStatus = def,
        veRelated = def,
        veResources = def,
        veRDate = def,
        veAlarms = def,
        veOther = def
      }
  where
    _englishList :: [TL.Text] -> Maybe TL.Text
    _englishList [] = Nothing
    _englishList [x] = Just x
    _englishList [x, y] = Just $ x <> " and " <> y
    _englishList xs = Just $ TL.intercalate ", " (init xs) <> ", and " <> last xs
    _timeString :: UTCTime -> TL.Text
    _timeString = TL.pack . formatTime defaultTimeLocale "%B %d, %Y %H:%M UTC"

getSummary :: (AspectKind, Almanac.Event) -> TL.Text
getSummary = TL.pack . eventString

astrologicalCalendar :: AstrologicalEvents -> IO VCalendar
astrologicalCalendar events = do
  delineations <- getDelineations
  vEvents <- mapM (uncurry $ eventVEvent delineations) events
  return $
    def
      { vcEvents = Map.fromList (map (\e -> ((uidValue (veUID e), Nothing), e)) vEvents)
      }
