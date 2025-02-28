module AstroCalendar.ICalendar where

import AstroCalendar.Event
import AstroCalendar.Types
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Text.ICalendar

astrologicalCalendar :: AstrologicalEvents -> IO VCalendar
astrologicalCalendar (retrogradePeriods, signPeriods, aspectPeriods, transitPeriods, eclipses) = do
  signVEvents <- traverse makeVEvent (fromMaybe [] signPeriods)
  retrogradeVEvents <- traverse makeVEvent (fromMaybe [] retrogradePeriods)
  aspectVEvents <- traverse makeVEvent (fromMaybe [] aspectPeriods)
  transitVEvents <- traverse makeVEvent (fromMaybe [] transitPeriods)
  eclipseVEvents <- traverse makeVEvent (fromMaybe [] eclipses)
  let events = signVEvents ++ retrogradeVEvents ++ aspectVEvents ++ transitVEvents ++ eclipseVEvents
  return $
    def
      { vcEvents = Map.fromList (map (\e -> ((uidValue (veUID e), Nothing), e)) events)
      }

makeVEvent :: (IsEvent e) => e -> IO VEvent
makeVEvent e = do
  uuid <- TL.fromStrict . UUID.toText <$> UUID.nextRandom
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
            (description e),
        veSummary =
          Just
            ( Summary
                { summaryValue = summary e,
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
            DTStartDateTime
              { dtStartDateTimeValue = UTCDateTime (startTime e),
                dtStartOther = def
              },
        veDTEndDuration =
          Just
            ( Left
                DTEndDateTime
                  { dtEndDateTimeValue = UTCDateTime (endTime e),
                    dtEndOther = def
                  }
            ),
        veTransp = Transparent {timeTransparencyOther = def},
        veAlarms = def,
        veAttach = def,
        veAttendee = def,
        veCategories = def,
        veClass = def,
        veComment = def,
        veContact = def,
        veCreated = def,
        veExDate = def,
        veGeo = def,
        veLastMod = def,
        veLocation = def,
        veOrganizer = def,
        veOther = def,
        vePriority = def,
        veRDate = def,
        veRRule = def,
        veRStatus = def,
        veRecurId = def,
        veRelated = def,
        veResources = def,
        veSeq = def,
        veStatus = def,
        veUrl = def
      }
