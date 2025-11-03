{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.ICalendar where

import AstroCalendar.Delineation (Delineations, getDelineations)
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
import Control.Applicative ((<|>))

astrologicalCalendar :: Precision -> AstrologicalEvents -> IO VCalendar
astrologicalCalendar precision (retrogradePeriods, signPeriods, aspectPeriods, transitPeriods, eclipses) = do
  delineations <- getDelineations
  signVEvents <- traverse makeVEvent' (fromMaybe [] signPeriods)
  retrogradeVEvents <- traverse makeVEvent' (fromMaybe [] retrogradePeriods)
  aspectVEvents <- traverse (makeAspectVEvent delineations) (fromMaybe [] aspectPeriods)
  transitVEvents <- traverse makeVEvent' (fromMaybe [] transitPeriods)
  eclipseVEvents <- traverse makeVEvent' (fromMaybe [] eclipses)
  let events = signVEvents ++ retrogradeVEvents ++ aspectVEvents ++ transitVEvents ++ eclipseVEvents
  return $
    def
      { vcEvents = Map.fromList (map (\e -> ((uidValue (veUID e), Nothing), e)) events)
      }
  where
    englishList :: [TL.Text] -> Maybe TL.Text
    englishList [] = Nothing
    englishList [x] = Just x
    englishList [x, y] = Just $ x <> " and " <> y
    englishList xs = Just $ TL.intercalate ", " (init xs) <> ", and " <> last xs
    makeAspectVEvent :: Delineations -> AspectEvent NatalAspect -> IO VEvent
    makeAspectVEvent delineations e = do
      let exactTimes = [aspectExactTime e]
          timeString :: UTCTime -> TL.Text
          -- TODO zone the time correctly
          timeString = TL.pack . formatTimeWithPrecision precision
      let delineation = eventSummary delineations e
          description =
            TL.unlines $
              catMaybes
                [ delineation,
                  fmap ((<> ".") . ("Most intense around " <>)) (englishList (map timeString exactTimes)) <|> Just "Does not go exact."
                ]
      makeVEvent (Just description) e
    makeVEvent' :: (IsEvent e) => e -> IO VEvent
    makeVEvent' = makeVEvent Nothing

eventSummary :: Delineations -> AspectEvent k -> Maybe TL.Text
eventSummary delineations e =
  case e of
    AspectEvent {aspect = Aspect {point1 = Planet p1, point2 = Planet p2, aspectType}} ->
      delineations (min p1 p2, max p1 p2, aspectType)
    _ -> Nothing

makeVEvent :: (IsEvent e) => Maybe TL.Text -> e -> IO VEvent
makeVEvent description e = do
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
            description,
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
