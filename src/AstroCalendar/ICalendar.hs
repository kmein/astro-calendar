{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AstroCalendar.ICalendar where

import AstroCalendar.Delineation (Delineations, getDelineations)
import AstroCalendar.Event
import AstroCalendar.ExactTime (findExactTimes)
import AstroCalendar.Types
import Control.Applicative ((<|>))
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

astrologicalCalendar :: Precision -> AstrologicalEvents -> IO VCalendar
astrologicalCalendar precision (retrogradePeriods, signPeriods, aspectPeriods, transitPeriods, eclipses) = do
  delineations <- getDelineations
  signVEvents <- traverse (makeVEvent precision Nothing) (fromMaybe [] signPeriods)
  retrogradeVEvents <- traverse (makeVEvent precision Nothing) (fromMaybe [] retrogradePeriods)
  aspectVEvents <- traverse (makeAspectVEvent delineations) (fromMaybe [] aspectPeriods)
  transitVEvents <- traverse (makeVEvent precision Nothing) (fromMaybe [] transitPeriods)
  eclipseVEvents <- traverse (makeVEvent precision Nothing) (fromMaybe [] eclipses)
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
    timeString :: UTCTime -> TL.Text
    timeString = TL.pack . formatTime defaultTimeLocale "%B %d, %Y %H:%M UTC"
    makeAspectVEvent :: Delineations -> AspectEvent NatalAspect -> IO VEvent
    makeAspectVEvent delineations e = do
      exactTimes <- findExactTimes e
      let delineation = eventSummary delineations e
          description =
            TL.unlines $
              catMaybes
                [ delineation,
                  fmap ((<> ".") . ("Going exact on " <>)) (englishList (map timeString exactTimes)) <|> Just "Does not go exact."
                ]
      makeVEvent precision (Just description) e

eventSummary :: Delineations -> AspectEvent k -> Maybe TL.Text
eventSummary delineations e =
  case e of
    AspectEvent {aspect = Aspect {point1 = Planet p1, point2 = Planet p2, aspectType}} ->
      Map.lookup (min p1 p2, max p1 p2, aspectType) delineations
    _ -> Nothing

makeVEvent :: (IsEvent e) => Precision -> Maybe TL.Text -> e -> IO VEvent
makeVEvent precision description e = do
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
          Just $ case precision of
            AstroCalendar.Types.Daily ->
              DTStartDate
                { dtStartDateValue = Date (utctDay (startTime e)),
                  dtStartOther = def
                }
            _ ->
              DTStartDateTime
                { dtStartDateTimeValue = UTCDateTime (startTime e),
                  dtStartOther = def
                },
        veDTEndDuration =
          Just $ Left $ case precision of
            AstroCalendar.Types.Daily ->
              DTEndDate
                { dtEndDateValue = Date (addDays 1 (utctDay (endTime e))),
                  dtEndOther = def
                }
            _ ->
              DTEndDateTime
                { dtEndDateTimeValue = UTCDateTime (endTime e),
                  dtEndOther = def
                },
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
