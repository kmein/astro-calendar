{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.ICalendar where

import Almanac qualified
import AstroCalendar.Delineation (Delineations, getDelineationFor, getDelineations)
import AstroCalendar.Event
import AstroCalendar.Types
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Text.ICalendar

eventVEvent :: Delineations -> (AspectKind, Almanac.Event, Maybe [UTCTime]) -> IO VEvent
eventVEvent delineations (natalOrMundane, event, exactitudeMoments) = do
  uuid <- TL.fromStrict . UUID.toText <$> UUID.nextRandom
  let description = TL.unlines $ catMaybes [delineation, exactTimes]
      delineation = getDelineationFor delineations (natalOrMundane, event)
      summary = getSummary (natalOrMundane, event)
      exactTimes = fmap ("Exact at " <>) . englishList . map timeString =<< exactitudeMoments
  pure $
    VEvent
      { veDescription =
          Just
            Description
              { descriptionValue = description,
                descriptionAltRep = def,
                descriptionLanguage = def,
                descriptionOther = def
              },
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
    englishList :: [TL.Text] -> Maybe TL.Text
    englishList [] = Nothing
    englishList [x] = Just x
    englishList [x, y] = Just $ x <> " and " <> y
    englishList xs = Just $ TL.intercalate ", " (init xs) <> ", and " <> last xs
    timeString :: UTCTime -> TL.Text
    timeString = TL.pack . formatTime defaultTimeLocale "%B %d, %Y %H:%M UTC"

getSummary :: (AspectKind, Almanac.Event) -> TL.Text
getSummary = TL.pack . eventString

astrologicalCalendar :: AstrologicalEvents -> IO VCalendar
astrologicalCalendar events = do
  delineations <- getDelineations
  vEvents <- mapM (eventVEvent delineations) events
  return $
    def
      { vcEvents = Map.fromList (map (\e -> ((uidValue (veUID e), Nothing), e)) vEvents)
      }
