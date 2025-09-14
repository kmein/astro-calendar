{-# LANGUAGE TypeApplications #-}

module Main where

import AstroCalendar.Angle
import AstroCalendar.Aspect
import AstroCalendar.Types
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import SwissEphemeris

findExactTime :: AspectEvent k -> IO UTCTime
findExactTime (AspectEvent {aspectStartTime, aspectEndTime, aspect = Aspect {point1 = Planet p1, point2 = Planet p2, aspectType}}) = do
  roots <-
    searchRoots
      (aspectFunc p1 p2)
      (degrees (distanceDegrees aspectType))
      (aspectStartTime, aspectEndTime)
  case roots of
    (t : _) -> return t
    [] -> error "No root found in interval"
findExactTime _ = error "Not an aspect event"

main :: IO ()
main = do
  rawResults <-
    findExactTime
      AspectEvent
        { aspect = Aspect {point1 = Planet Sun, point2 = Planet Saturn, aspectType = Sextile},
          aspectStartTime = UTCTime (fromGregorian 2025 1 1) 0,
          aspectEndTime = UTCTime (fromGregorian 2025 1 10) 0,
          aspectExactTime = undefined
        }
  print rawResults

-- | Generic Newton + bisection refinement
refineRoot ::
  (UTCTime -> IO (Double, Double)) -> -- f(t), f'(t)
  Double -> -- target value
  (UTCTime, UTCTime) -> -- initial bracket (low, high)
  IO UTCTime
refineRoot f target (tLow, tHigh) = go mid 50 (tLow, tHigh)
  where
    tol = 1e-5 * nominalDay -- ~0.86 sec
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

          putStrLn $ "Refining at " ++ show t ++ ": f(t) = " ++ show fval ++ ", f'(t) = " ++ show deriv ++ ", step = " ++ show step ++ "s, Newton = " ++ show tNewton

          -- Midpoint bisection candidate
          let tBisect = addUTCTime ((diffUTCTime hi lo) / 2) lo
              tNext =
                if tNewton <= lo || tNewton >= hi
                  then tBisect -- reject runaway Newton, fallback to bisection
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
aspectFunc :: Planet -> Planet -> UTCTime -> IO (Double, Double)
aspectFunc p1 p2 utc = do
  Just jd <- toJulianDay utc
  pos1 <- calculateEclipticPosition jd p1 >>= either (error) return
  pos2 <- calculateEclipticPosition jd p2 >>= either (error) return
  putStrLn $ "  " ++ show p1 ++ ": " ++ show (lng pos1) ++ ", " ++ show p2 ++ ": " ++ show (lng pos2)

  let lon1 = Angle $ lng pos1
      lon2 = Angle $ lng pos2
      spd1 = lngSpeed pos1
      spd2 = lngSpeed pos2
      diff = lon1 `difference` lon2
      diff' = spd1 - spd2
  -- inline angleDiff here and log intermediate values
  putStrLn $ "  lon1: " ++ show lon1 ++ ", lon2: " ++ show lon2
  putStrLn $ "  diff: " ++ show diff ++ ", diff': " ++ show diff'
  return (degrees $ abs diff, diff')

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
  let lon = Angle (lng pos)
      spd = lngSpeed pos
      val = lon `difference` fromIntegral targetSign * Angle 30
  return (degrees val, spd)

-- | Generic root search over an interval
searchRoots ::
  (UTCTime -> IO (Double, Double)) -> -- f(t), f'(t)
  Double -> -- target
  (UTCTime, UTCTime) -> -- interval
  IO [UTCTime]
searchRoots f target (t0, t1) = loop t0 Nothing []
  where
    step = nominalDay / 2 -- 0.5 day
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
              if (prevVal - target) * (val - target) <= 0 -- sign change
                then do
                  let _mid = addUTCTime (diffUTCTime t tPrev / 2) tPrev
                  root <- refineRoot f target (tPrev, t)
                  putStrLn $ "  Found root at " ++ show root
                  loop tNext (Just (val, t)) (root : acc)
                else loop tNext (Just (val, t)) acc

type FixedPoint = (String, Angle) -- name and ecliptic longitude in degrees

transitToFixed ::
  Planet -> FixedPoint -> AspectType -> UTCTime -> IO (Double, Double)
transitToFixed p (_name, lonFixed) aspect utc = do
  Just jd <- toJulianDay utc
  Right pos <- calculateEclipticPosition jd p
  let lonTrans = Angle $ lng pos
      angle = distanceDegrees aspect
      spd = lngSpeed pos
      diff = lonTrans `difference` lonFixed
  return (degrees $ abs diff - angle, spd)
