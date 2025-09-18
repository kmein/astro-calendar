{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Event (astrologicalEvents, AstrologicalEvents, getEvent) where

import Almanac qualified
import AstroCalendar.Types
import Data.Foldable (toList)
import Data.Foldable1 (toNonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time

type AstrologicalEvents = [(AspectKind, Almanac.Event, Maybe [UTCTime])]

getEvent :: Either Almanac.Event Almanac.ExactEvent -> Almanac.Event
getEvent = either id Almanac.event

astrologicalEvents :: SelectionOptions -> EventsSettings -> IO AstrologicalEvents
astrologicalEvents options settings = do
  let interval = uncurry Almanac.Interval $ dateRange settings
      transitOptions =
        Almanac.TransitOptions
          { Almanac.includeTransitProgress = True,
            Almanac.transitAspects = toNonEmpty $ allAspectTypes options,
            Almanac.transitTargets =
              NonEmpty.fromList
                [ (p1, p2)
                  | p1 <- toList $ allPlanets options,
                    p2 <- toList $ allPlanets options,
                    p1 < p2
                ]
          }
      mundaneQuery =
        Almanac.mundane interval $
          NonEmpty.fromList $
            concat
              [ [Almanac.QueryPlanetaryMundaneTransit transitOptions | withAspects settings],
                [Almanac.QueryZodiacIngress (allPlanets options) | withSigns settings],
                [Almanac.QueryDirectionChange (allPlanets options) | withRetrograde settings],
                [Almanac.QueryEclipse | withEclipses settings]
              ]
      natalQuery referenceEvent =
        Almanac.natal
          interval
          referenceEvent
          (NonEmpty.singleton (Almanac.QueryPlanetaryNatalTransit transitOptions))
      predicate = \case
        (_, Almanac.DirectionChange (Almanac.PlanetStation {Almanac.stationType}), _)
          | stationType == Almanac.Direct -> False
        _ -> True
      maybeExactify tag xs =
        if exactTimes settings
          then
            fmap
              ( \x ->
                  ( tag,
                    Almanac.event x,
                    Just $ Almanac.exactitudeMoments x
                  )
              )
              <$> Almanac.eventsWithExactitude xs
          else return $ fmap (\x -> (tag, x, Nothing)) xs
      getMundaneEvents = Almanac.runQuery mundaneQuery
      getNatalEvents = case (transitsTo settings, position options) of
        (Just referenceDate, Just position) ->
          let referenceEvent =
                ( Almanac.ReferenceEvent
                    { Almanac.eventTime = referenceDate,
                      Almanac.eventLocation = position
                    }
                )
           in Almanac.runQuery (natalQuery referenceEvent)
        _ -> pure mempty
   in fmap (filter (predicate) . toList) $
        (<>)
          <$> (maybeExactify Mundane =<< getMundaneEvents)
          <*> (maybeExactify Natal =<< getNatalEvents)
