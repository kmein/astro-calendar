{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module AstroCalendar.ExactTime (findExactTimes, findStations, getEclipseMax, findCrossings) where

import AstroCalendar.Angle
import AstroCalendar.Aspect
import AstroCalendar.Types
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import SwissEphemeris

findExactTimes :: AspectEvent k -> IO [UTCTime]
findExactTimes (AspectEvent {aspectStartTime, aspectEndTime, aspect = Aspect {point1 = Planet p1, point2 = Planet p2, aspectType}}) =
  searchRoots
    (angleBetween p1 p2)
    (degrees (distanceDegrees aspectType))
    (aspectStartTime, aspectEndTime)
findExactTimes _ = return []

getEclipseMax :: EclipseEvent -> IO UTCTime
getEclipseMax =
  fromJulianDay . \case
    LunarEclipse e -> lunarEclipseMax e
    SolarEclipse e -> solarEclipseMax e

findStations :: Planet -> Integer -> IO [UTCTime]
findStations planet year = do
  let t0 = UTCTime (fromGregorian year 1 1) 0
      t1 = UTCTime (fromGregorian (year + 1) 1 1) 0
  roots <-
    searchRoots
      (planetSpeed planet)
      0
      (t0, t1)
  return roots

findCrossings :: Planet -> Angle -> Integer -> IO [UTCTime]
findCrossings planet angle year = do
  let t0 = UTCTime (fromGregorian year 1 1) 0
      t1 = UTCTime (fromGregorian (year + 1) 1 1) 0
  roots <-
    searchRoots
      (angleToPoint planet angle)
      0
      (t0, t1)
  return roots

-- | Bisection refinement
refineRoot ::
  (UTCTime -> IO Double) -> -- f(t)
  Double -> -- target value
  (UTCTime, UTCTime) -> -- initial bracket (low, high)
  IO UTCTime
refineRoot f target (tLow, tHigh) = go 50 mid (tLow, tHigh)
  where
    tol = 1e-5 * nominalDay -- ~0.86 sec
    eps = 1e-6
    midT t0 t1 = addUTCTime ((diffUTCTime t1 t0) / 2) t0
    mid = midT tLow tHigh

    go :: Int -> UTCTime -> (UTCTime, UTCTime) -> IO UTCTime
    go n t (lo, hi)
      | n <= 0 = return t
      | otherwise = do
      val <- f t
      valLo <- f lo
      let fval = val - target
          fLo = valLo - target
          tNext = midT lo hi
          (newLo, newHi) =
            if fLo * fval <= 0 -- sign change between lo and t
              then (lo, t) -- root is between lo and t
              else (t, hi) -- root is between t and hi
      if abs (realToFrac @_ @Double (diffUTCTime newHi newLo)) < realToFrac tol -- interval is sufficiently small
        && abs fval < eps -- function value is sufficiently close to target
        then return tNext
        else go (pred n) tNext (newLo, newHi)

-- | Generic root search over an interval
searchRoots ::
  (UTCTime -> IO Double) ->
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
          -- putStrLn $ "Scanning at " ++ show t
          val <- f t
          let tNext = min (addUTCTime step t) t1
          -- putStrLn $ "  f(t) = " ++ show val
          case prev of
            Nothing -> loop tNext (Just (val, t)) acc
            Just (prevVal, tPrev) ->
              if (prevVal - target) * (val - target) <= 0 -- sign change
                 || (abs (val - target) < 1e-1) -- close to zero
                then do
                  let _mid = addUTCTime (diffUTCTime t tPrev / 2) tPrev
                  root <- refineRoot f target (tPrev, t)
                  -- putStrLn $ "  Found root at " ++ show root
                  loop tNext (Just (val, t)) (root : acc)
                else loop tNext (Just (val, t)) acc

-- | Aspect function
angleBetween :: Planet -> Planet -> UTCTime -> IO (Double)
angleBetween p1 p2 utc = do
  Just jd <- toJulianDay utc
  pos1 <- calculateEclipticPosition jd p1 >>= either (error) return
  pos2 <- calculateEclipticPosition jd p2 >>= either (error) return
  let lon1 = Angle $ lng pos1
      lon2 = Angle $ lng pos2
      diff = lon1 `difference` lon2
  return (degrees $ abs diff)

-- | Station function
planetSpeed :: Planet -> UTCTime -> IO Double
planetSpeed p utc = do
  Just jd <- toJulianDay utc
  pos <- calculateEclipticPosition jd p >>= either (error) return
  let spd = lngSpeed pos
  return (spd)

angleToPoint :: Planet -> Angle -> UTCTime -> IO (Double)
angleToPoint p angle utc = do
  Just jd <- toJulianDay utc
  pos <- calculateEclipticPosition jd p >>= either (error) return
  let lon = Angle (lng pos)
      val = lon `difference` angle
  return (degrees val)
