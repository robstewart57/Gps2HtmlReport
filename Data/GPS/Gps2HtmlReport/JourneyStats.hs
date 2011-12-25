-- | This module provides the JourneyCharts and HTMLGenerator 
-- modules with statistics for the charts, and the journey statistics
module Data.GPS.Gps2HtmlReport.JourneyStats (
  ptsElevation,     -- :: [WptType] -> [(LocalTime,Double)]
  avrSpeedOverTime, -- :: [(LocalTime,Speed)] -> Double -> Double -> [(LocalTime,Speed)] -> [(LocalTime,Speed)]
  accumDistance,    -- :: [WptType] -> Double -> [(LocalTime,Distance)]
  findPoint,        -- findPoint :: [WptType] -> WptType -> (WptType -> Maybe Double) -> (Double -> Double -> Bool) -> Maybe (LocalTime,Double)
  journeyDistance,  -- :: (Lat a, Lon a) => [a] -> Distance
  meanElevation,    -- :: Ele a => [a] -> Double
  journeyTime,      -- :: Time a => [a] -> NominalDiffTime
  maxSpeed,         -- :: [WptType] -> Speed
  meanJourneySpeed, -- :: (Lat a, Lon a, Time a) => [a] -> Distance
  dateOfJourney     -- :: Time a => [a] -> Maybe Day
  ) where

import Data.GPS
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Text.XML.XSD.DateTime

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,Elevation)
ptsElevation :: [WptType] -> [(LocalTime,Double)]
ptsElevation = map (\point -> (utcToLocalTime dfltTZ (Text.XML.XSD.DateTime.toUTCTime (fromJust $ time point)) , fromJust $ ele point))

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,AvrSpeedAtThisPoint)
avrSpeedOverTime :: [(LocalTime,Speed)] -> Double -> Double -> [(LocalTime,Speed)] -> [(LocalTime,Speed)]
avrSpeedOverTime [] _ _ _ = []
avrSpeedOverTime [spd] totalSpeed numPoints iteratedAvr = iteratedAvr ++ [(fst spd, (snd spd + totalSpeed) / (numPoints+1))]
avrSpeedOverTime (spd:spds) totalSpeed numPoints iteratedAvr = avrSpeedOverTime [spd] totalSpeed numPoints iteratedAvr ++ avrSpeedOverTime spds (snd spd + totalSpeed) (numPoints+1) iteratedAvr

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,SpeedAtThisPoint) 
speedAtPoints :: [WptType] -> [(LocalTime,Speed)]
speedAtPoints [] = []
speedAtPoints (point:points) = (lclTime $ fromJust (time point),0.0) : speedAtPoints' point points

speedAtPoints' :: WptType -> [WptType] -> [(LocalTime, Speed)]
speedAtPoints' _ [] = []
speedAtPoints' prev [x]
         | isJust (time x) = [(lclTime $ fromJust (time x), fromJust $ speed prev x)]
         | otherwise = []
speedAtPoints' prev (x:xs)
         | isJust (time x) && isJust (speed prev x) = (lclTime $ fromJust (time x), fromJust $ speed prev x) : speedAtPoints' x xs
         | otherwise = [] ++ speedAtPoints' x xs

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,JourneyDistanceAtPoint)
accumDistance :: [WptType] -> Double -> [(LocalTime,Distance)]
accumDistance [] _ = []
accumDistance [x] _ = [(lclTime $ fromJust (time x),0.0)]
accumDistance (x:xs) acc = 
   let dist = distance x (head xs)           
   in (lclTime $ fromJust (time x), dist + acc ) : accumDistance xs (dist + acc)

-- | Takes all WayPoints, an element in wptType, and an Eq function, returning a single WayPoint
findPoint :: [WptType] -> WptType -> (WptType -> Maybe Double) -> (Double -> Double -> Bool) -> Maybe (LocalTime,Double)
findPoint [] _ _ _ = Nothing
findPoint (point:points) currSelected wayPointElement equalityF
   | equalityF (fromJust $ wayPointElement point) (fromJust $ wayPointElement currSelected) = findPoint points point wayPointElement equalityF
   | otherwise = if null points
                      then Just (lclTime (fromJust $ time currSelected), fromJust $ ele currSelected)
                      else findPoint points currSelected wayPointElement equalityF

-- | Calculates the total journey distance
journeyDistance :: (Lat a, Lon a) => [a] -> Distance
journeyDistance [] = 0.0
journeyDistance [_] = 0.0
journeyDistance (point:points) = distance point (head points) + journeyDistance points

-- | Calculates the average speed of the journey
meanJourneySpeed :: (Lat a, Lon a, Time a) => [a] -> Distance
meanJourneySpeed points = journeyDistance points / realToFrac (journeyTime points)

-- | Calculates the maximum speed
maxSpeed :: [WptType] -> Speed
maxSpeed points =
            let speedTuple = speedAtPoints points
                speedList = map snd speedTuple
                maxSpeed = foldr max 0.0 speedList
            in maxSpeed

-- | Calculates the average elevation throughout the journey
meanElevation :: Ele a => [a] -> Double
meanElevation points = 
            let elevationVals = map (fromJust . ele) points
                totalElevation = foldr (+) 0.0 elevationVals
                theMean = totalElevation / fromIntegral (length points)
            in theMean

-- | Calculates the total journey time
journeyTime :: Time a => [a] -> NominalDiffTime
journeyTime [] = 0
journeyTime [_] = 0
journeyTime (point:points) = 
             let startTime = toUTCTime (fromJust (time point))
                 endTime = toUTCTime (fromJust (time $ last points))
             in diffUTCTime endTime startTime

-- | Extracts the date of the journey (from the first WayPoint)
dateOfJourney :: Time a => [a] -> Maybe Day
dateOfJourney [] = Nothing
dateOfJourney (point:_) = Just $ utctDay $ toUTCTime $ fromJust (time point)

lclTime :: DateTime -> LocalTime
lclTime dteTime = utcToLocalTime dfltTZ (Text.XML.XSD.DateTime.toUTCTime dteTime)

dfltTZ :: TimeZone
dfltTZ = TimeZone {timeZoneMinutes=0,timeZoneSummerOnly=False,timeZoneName="GMT"}
