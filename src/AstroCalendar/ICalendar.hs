module AstroCalendar.ICalendar where

import AstroCalendar.Aspect (findAspects)
import AstroCalendar.Ephemeris (parallelEphemeris)
import AstroCalendar.Event
import AstroCalendar.Types
import Control.Arrow (second)
import Data.Default
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import SwissEphemeris qualified as SwE
import Text.ICalendar

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
            { dtStampValue = UTCTime (fromGregorian 1970 1 1) 1.1,
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

astrologicalEvents :: Map.Map SwE.Planet Ephemeris -> IO [VEvent]
astrologicalEvents ephemerides =
  let retrogradePeriods = Map.mapWithKey retrogradeEvents ephemerides
      signPeriods = Map.mapWithKey signEvent ephemerides
      aspectPeriods = aspectEvents $ map (second findAspects) $ parallelEphemeris ephemerides
   in do
        signVEvents <- traverseMany makeVEvent signPeriods
        retrogradeVEvents <- traverseMany makeVEvent retrogradePeriods
        aspectVEvents <- traverse makeVEvent aspectPeriods
        pure $ signVEvents ++ retrogradeVEvents ++ aspectVEvents
  where
    traverseMany :: (Applicative f, Traversable t) => (a -> f b) -> t [a] -> f [b]
    traverseMany f = fmap concat . traverse (traverse f)

astrologicalCalendar :: Map.Map SwE.Planet Ephemeris -> IO VCalendar
astrologicalCalendar ephemerides = do
  events <- astrologicalEvents ephemerides
  return $ def {vcEvents = Map.fromList (map (\e -> ((uidValue (veUID e), Nothing), e)) events)}
