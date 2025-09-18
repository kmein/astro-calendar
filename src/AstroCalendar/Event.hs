{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module AstroCalendar.Event (astrologicalEvents, AstrologicalEvents) where

import Almanac qualified
import AstroCalendar.Types
import Data.Foldable (toList)
import Data.Foldable1 (toNonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

type AstrologicalEvents = [(AspectKind, Almanac.Event)]

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
        (_, Almanac.DirectionChange (Almanac.PlanetStation {Almanac.stationType}))
          | stationType == Almanac.Direct -> False
        _ -> True
      getMundaneEvents = map (Mundane,) . toList <$> Almanac.runQuery mundaneQuery
      getNatalEvents = case (transitsTo settings, position options) of
        (Just referenceDate, Just position) ->
          let referenceEvent =
                ( Almanac.ReferenceEvent
                    { Almanac.eventTime = referenceDate,
                      Almanac.eventLocation = position
                    }
                )
           in map (Natal,) . toList <$> Almanac.runQuery (natalQuery referenceEvent)
        _ -> pure []
   in fmap (filter predicate) $ (++) <$> getMundaneEvents <*> getNatalEvents
