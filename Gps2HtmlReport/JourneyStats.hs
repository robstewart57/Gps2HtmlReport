-- | This module provides the JourneyCharts and HTMLGenerator 
-- modules with statistics for the charts, and the journey statistics
module Gps2HtmlReport.JourneyStats where

import Data.GPS hiding (speed)
import Data.Maybe
import System.Random
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Text.XML.XSD.DateTime

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,Elevation)
ptsElevation :: [WptType] -> [(LocalTime,Double)]
ptsElevation = map (\point -> (utcToLocalTime dfltTZ (Text.XML.XSD.DateTime.toUTCTime (fromJust $ time point)) , fromJust $ ele point))

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,AvrSpeedAtThisPoint)
avrSpeedOverTime :: [(LocalTime,Speed)] -> Double -> Double -> [(LocalTime,Speed)] -> [(LocalTime,Speed)]
avrSpeedOverTime [spd] totalSpeed numPoints iteratedAvr = iteratedAvr ++ [(fst spd, (snd spd + totalSpeed) / (numPoints+1))]
avrSpeedOverTime (spd:spds) totalSpeed numPoints iteratedAvr = avrSpeedOverTime [spd] totalSpeed numPoints iteratedAvr ++ avrSpeedOverTime spds (snd spd + totalSpeed) (numPoints+1) iteratedAvr

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,SpeedAtThisPoint) 
speedAtPoints :: [WptType] -> [(LocalTime,Speed)]
speedAtPoints points = speedAtPoints' (head points) (tail points)

speedAtPoints' :: WptType -> [WptType] -> [(LocalTime, Speed)]
speedAtPoints' prev [x]
         | isJust (time x) = [(lclTime $ fromJust (time x), fromJust $ speed prev x)]
         | otherwise = []
speedAtPoints' prev (x:xs)
         | isJust (time x) = (lclTime $ fromJust (time x), fromJust $ speed prev x) : speedAtPoints' x xs
         | otherwise = [] ++ speedAtPoints' x xs

-- | Takes all WayPoints, and creates a list of tuples containing (TimeStamp,JourneyDistanceAtPoint)
accumDistance :: [WptType] -> Double -> [(LocalTime,Distance)]
accumDistance [x] acc = [(lclTime $ fromJust (time x),0.0)]
accumDistance (x:xs) acc = 
   let dist = distance x (head xs)           
   in (lclTime $ fromJust (time x), dist + acc ) : accumDistance (tail xs) (dist + acc)

-- | Takes all WayPoints, an element in wptType, and an Eq function, returning a single WayPoint
findPoint :: [WptType] -> WptType -> (WptType -> Maybe Double) -> (Double -> Double -> Bool) -> (LocalTime,Double)
findPoint (point:points) currSelected wayPointElement equalityF
   | equalityF (fromJust $ wayPointElement point) (fromJust $ wayPointElement currSelected) = findPoint points point wayPointElement equalityF
   | otherwise = if null points
                      then (lclTime (fromJust $ time currSelected), fromJust $ ele currSelected)
                      else findPoint points currSelected wayPointElement equalityF

-- | Calculates the total journey distance
journeyDistance :: (Lat a, Lon a) => [a] -> Distance
journeyDistance [point] = 0.0
journeyDistance (point:points) = distance point (head points) + journeyDistance points

-- | Calculates the average speed of the journey
meanJourneySpeed :: (Lat a, Lon a) => [a] -> Distance
meanJourneySpeed points = journeyDistance points / fromIntegral (length points)

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
journeyTime points = 
             let startTime = toUTCTime (fromJust (time $ head points))
                 endTime = toUTCTime (fromJust (time $ last points))
             in diffUTCTime endTime startTime

-- | Extracts the date of the journey (from the first WayPoint)
dateOfJourney :: Time a => [a] -> Day
dateOfJourney points = utctDay $ toUTCTime $ fromJust (time $ head points)

lclTime :: DateTime -> LocalTime
lclTime dteTime = utcToLocalTime dfltTZ (Text.XML.XSD.DateTime.toUTCTime dteTime)

dfltTZ :: TimeZone
dfltTZ = TimeZone {timeZoneMinutes=0,timeZoneSummerOnly=False,timeZoneName="GMT"}

-- | Overides the `speed' function in the `gps' package
speed :: (Lat loc, Lon loc, Time loc) => loc -> loc -> Maybe Speed
speed a b = 
	case (getUTCTime b, getUTCTime a) of
		(Just x, Just y) -> Just $ distance a b / realToFrac (diffUTCTime x y) 
		_ -> Nothing

getUTCTime :: (Lat a, Lon a, Time a) => a -> Maybe UTCTime
getUTCTime = fmap toUTCTime . time