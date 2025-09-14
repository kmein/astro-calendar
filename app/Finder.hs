{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Fixed (mod')
import Data.List (groupBy, sortOn)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import SwissEphemeris

main :: IO ()
main = do
  -- examplePlanets
  let t0 = UTCTime (fromGregorian 2024 1 1) 0
      t1 = UTCTime (fromGregorian 2025 1 10) 0
      step = nominalDay / 2-- 1 day
      tol = 1e-5 * nominalDay -- ~0.86 sec
      p1 = Sun
      p2 = Saturn
      angle = 60 -- Sextile
  rawResults <- searchRoots (aspectFunc p1 p2 angle) 0 step tol t0 t1
  print rawResults

-- | Normalize angle to [0,360)
normAngle :: Double -> Double
normAngle angle =
  let angle' = angle `mod'` 360
   in if angle' < 0 then angle' + 360 else angle'

-- | Signed difference between two angles (-180..+180)
angleDiff :: Double -> Double -> Double
angleDiff a b =
  let d = normAngle (a - b)
   in if d > 180 then d - 360 else if d < -180 then d + 360 else d

-- | Generic Newton + bisection refinement
refineRoot ::
  (UTCTime -> IO (Double, Double)) -> -- f(t), f'(t)
  Double ->                          -- target value
  NominalDiffTime ->                 -- tolerance (seconds)
  (UTCTime, UTCTime) ->              -- initial bracket (low, high)
  IO UTCTime
refineRoot f target tol (tLow, tHigh) = go mid 40 (tLow, tHigh)
  where
    eps = 1e-6
    mid = addUTCTime ((diffUTCTime tHigh tLow) / 2) tLow

    go :: UTCTime -> Int -> (UTCTime, UTCTime) -> IO UTCTime
    go t n (lo, hi)
      | n <= 0 = do
          putStrLn $ "Max iterations reached, returning midpoint " ++ show mid
          return mid
      | otherwise = do
          (val, deriv) <- f t
          let fval = val - target
              derivSec = deriv / 86400
              step = realToFrac (-fval / derivSec)
              tNewton = addUTCTime step t

          putStrLn $
            "Refining at " ++ show t
              ++ ": f(t) = " ++ show fval
              ++ ", f'(t) = " ++ show deriv
              ++ ", step = " ++ show step
              ++ "s, Newton = " ++ show tNewton

          -- Midpoint bisection candidate
          let tBisect = addUTCTime ((diffUTCTime hi lo) / 2) lo
              tNext =
                if tNewton <= lo || tNewton >= hi
                   then tBisect  -- reject runaway Newton, fallback to bisection
                   else tNewton

          -- update bracket
          (valLo, _) <- f lo
          let fLo = valLo - target
              (newLo, newHi) =
                if fLo * fval <= 0
                  then (lo, t)
                  else (t, hi)

          if abs (realToFrac @_ @Double (diffUTCTime newHi newLo)) < realToFrac tol
             && abs fval < eps
            then do
              putStrLn $ "Converged at " ++ show tNext
              return tNext
            else go tNext (n - 1) (newLo, newHi)

-- | Aspect function
aspectFunc :: Planet -> Planet -> Double -> UTCTime -> IO (Double, Double)
aspectFunc p1 p2 aspect utc = do
  Just jd <- toJulianDay utc
  pos1 <- calculateEclipticPosition jd p1 >>= either (error) return
  pos2 <- calculateEclipticPosition jd p2 >>= either (error) return
  putStrLn $ "  " ++ show p1 ++ ": " ++ show (lng pos1) ++ ", " ++ show p2 ++ ": " ++ show (lng pos2)

  let lon1 = lng pos1
      lon2 = lng pos2
      spd1 = lngSpeed pos1
      spd2 = lngSpeed pos2
      diff =
        let d = normAngle (lon1 - lon2)
         in if d > 180 then d - 360 else if d < -180 then d + 360 else d
      diff' = spd1 - spd2
  -- inline angleDiff here and log intermediate values
  putStrLn $ "  lon1: " ++ show lon1 ++ ", lon2: " ++ show lon2
  putStrLn $ "  diff: " ++ show diff ++ ", diff': " ++ show diff'
  return (abs diff - aspect, diff')

-- | Station function
stationFunc :: Planet -> UTCTime -> IO (Double, Double)
stationFunc p utc = do
  Just jd <- toJulianDay utc
  pos <- calculateEclipticPosition jd p >>= either (error) return
  let spd = lngSpeed pos
      delta = 1e-3 * nominalDay -- seconds
      utcNext = addUTCTime delta utc
  Just jdNext <- toJulianDay utcNext
  posNext <- calculateEclipticPosition jdNext p >>= either (error) return
  let spdNext = lngSpeed posNext
      acc = (spdNext - spd) / (realToFrac delta / 86400) -- deg/day^2
  return (spd, acc)

-- | Sign ingress function
signFunc :: Planet -> Int -> UTCTime -> IO (Double, Double)
signFunc p targetSign utc = do
  Just jd <- toJulianDay utc
  pos <- calculateEclipticPosition jd p >>= either (error) return
  let lon = normAngle (lng pos)
      spd = lngSpeed pos
      val = lon - fromIntegral targetSign * 30
  return (val, spd)

-- | Generic root search over an interval
searchRoots ::
  (UTCTime -> IO (Double, Double)) -> -- f(t), f'(t)
  Double -> -- target
  NominalDiffTime -> -- step (seconds)
  NominalDiffTime -> -- tolerance (seconds)
  UTCTime ->
  UTCTime -> -- interval
  IO [UTCTime]
searchRoots f target step tol t0 t1 = loop t0 Nothing []
  where
    loop :: UTCTime -> Maybe (Double, UTCTime) -> [UTCTime] -> IO [UTCTime]
    loop t prev acc
      | t >= t1 = return (reverse acc)
      | otherwise = do
          putStrLn $ "Scanning at " ++ show t
          (val, _) <- f t
          let tNext = min (addUTCTime step t) t1
          putStrLn $ "  f(t) = " ++ show val
          case prev of
            Nothing -> loop tNext (Just (val, t)) acc
            Just (prevVal, tPrev) ->
              if (prevVal - target) * (val - target) <= 0 -- || abs (val - target) < 1e-1
                then do
                  let _mid = addUTCTime (diffUTCTime t tPrev / 2) tPrev
                  root <- refineRoot f target tol (tPrev, t)
                  putStrLn $ "  Found root at " ++ show root
                  loop tNext (Just (val, t)) (root : acc)
                else loop tNext (Just (val, t)) acc

aspects :: [(String, Double)]
aspects =
  [ ("Conjunction", 0),
    ("Opposition", 180),
    ("Square", 90),
    ("Trine", 120),
    ("Sextile", 60)
  ]

scanAspects ::
  Planet -> Planet -> UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> IO [(String, UTCTime)]
scanAspects p1 p2 t0 t1 step tol = do
  let searchForAspect (name, angle) = do
        roots <- searchRoots (aspectFunc p1 p2 angle) 0 step tol t0 t1
        return $ map (\jd -> (name, jd)) roots
  results <- mapM searchForAspect aspects
  return $ concat results

planets :: [Planet]
planets = [Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto]

scanAllAspects ::
  UTCTime ->
  UTCTime ->
  NominalDiffTime ->
  NominalDiffTime ->
  IO [(Planet, Planet, String, UTCTime)]
scanAllAspects t0 t1 step tol = do
  results <- forM planets $ \p1 ->
    forM planets $ \p2 ->
      if p1 < p2
        then do
          aspectResults <- scanAspects p1 p2 t0 t1 step tol
          -- Clip results to [t0, t1]
          let clipped = filter (\(_, t) -> t >= t0 && t <= t1) aspectResults
          return $ map (\(name, t) -> (p1, p2, name, t)) clipped
        else return [] -- avoid duplicate pairs and self-pairs
        -- Flatten and sort by UTCTime
  return $ sortOn (\(_, _, _, t) -> t) $ concat $ concat results

-- | Remove "almost duplicates" within a threshold (e.g., 1 minute)
dedupe :: NominalDiffTime -> [(a, a, b, UTCTime)] -> [(a, a, b, UTCTime)]
dedupe eps xs =
  map head $ groupBy (\a b -> abs (diffUTCTime (getTime a) (getTime b)) < eps) (sortOn getTime xs)
  where
    getTime (_, _, _, t) = t

examplePlanets :: IO ()
examplePlanets = do
  let t0 = UTCTime (fromGregorian 2025 1 1) 0
      t1 = UTCTime (fromGregorian 2025 12 31) 0
      step = nominalDay -- 1 day
      tol = 1e-5 * nominalDay -- ~0.86 sec
      eps = 60 -- seconds: remove roots within 1 minute of each other
  rawResults <- scanAllAspects t0 t1 step tol
  let sorted = sortOn (\(_, _, _, t) -> t) rawResults
      unique = dedupe eps sorted

  forM_ unique $ \(p1, p2, aspect, t) ->
    putStrLn $ show p1 ++ " - " ++ show p2 ++ " " ++ aspect ++ " at " ++ show t

-- | Example usage
example :: IO ()
example = do
  let t0 = UTCTime (fromGregorian 2025 1 1) 0
      t1 = UTCTime (fromGregorian 2025 12 31) 0
      step = nominalDay -- 1 day in seconds
      tol = 1e-5 * nominalDay -- ~0.86 sec
  transits <- searchRoots (aspectFunc Mars Jupiter 90) 0.0 step tol t0 t1
  print transits

  stations <- searchRoots (stationFunc Mercury) 0.0 step tol t0 t1
  print stations

type FixedPoint = (String, Double) -- name and ecliptic longitude in degrees

natalPlanets :: [FixedPoint]
natalPlanets =
  [ ("Natal Sun", 120.0),
    ("Natal Moon", 250.5),
    ("Natal Mars", 310.0)
  ]

transitToFixed ::
  Planet -> FixedPoint -> Double -> UTCTime -> IO (Double, Double)
transitToFixed p (_name, lonFixed) aspect utc = do
  Just jd <- toJulianDay utc
  Right pos <- calculateEclipticPosition jd p
  let lonTrans = lng pos
      spd = lngSpeed pos
      diff = angleDiff (lonTrans - lonFixed) aspect
  return (diff, spd)

scanTransitToFixed ::
  Planet ->
  [FixedPoint] ->
  UTCTime ->
  UTCTime ->
  NominalDiffTime ->
  NominalDiffTime ->
  IO [(String, String, Double, UTCTime)]
scanTransitToFixed transiting fps t0 t1 step tol = do
  results <- forM fps $ \(name, lon) -> do
    aspectResults <- forM aspects $ \(_aspectName, angle) -> do
      roots <- searchRoots (transitToFixed transiting (name, lon) angle) 0 step tol t0 t1
      return $ map (\jd -> (transitingName transiting, name, angle, jd)) roots
    return $ concat aspectResults
  return $ concat results

-- Helper to get planet name
transitingName :: Planet -> String
transitingName Mercury = "Mercury"
transitingName Venus = "Venus"
transitingName Mars = "Mars"
transitingName Jupiter = "Jupiter"
transitingName Saturn = "Saturn"
transitingName Uranus = "Uranus"
transitingName Neptune = "Neptune"
transitingName Pluto = "Pluto"
transitingName Sun = "Sun"
transitingName Moon = "Moon"
transitingName _ = "Unknown"

exampleTransitFixed :: IO ()
exampleTransitFixed = do
  let t0 = UTCTime (fromGregorian 2025 1 1) 0
      t1 = UTCTime (fromGregorian 2025 12 31) 0
      step = nominalDay
      tol = 1e-5 * nominalDay
      eps = 60 -- remove duplicates within 1 minute
  rawResults <- scanTransitToFixed Mars natalPlanets t0 t1 step tol
  let sorted = sortOn (\(_, _, _, t) -> t) rawResults
      unique = dedupe eps sorted

  forM_ unique $ \(trans, natal, angle, t) ->
    putStrLn $ trans ++ " forms aspect " ++ show angle ++ "° with " ++ natal ++ " at " ++ show t
