module Main where

import Data.GPS
import System.FilePath
import System.Directory
import Text.Html

import Data.GPS.Gps2HtmlReport.HTMLGenerator
import Data.GPS.Gps2HtmlReport.JourneyCharts
import Data.GPS.Gps2HtmlReport.DrawOsm



-- | Reads the current directory for all .gpx files, then maps to `generateReport' for each one
main :: IO [()]
main = do
   curDir <- getCurrentDirectory
   allFiles <- getDirectoryContents curDir
   let allFilesSplit = map splitExtension allFiles
   let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
   putStr ("Processing "++show (length gpxFiles)++" file(s)...\n")
   mapM (\(a,b) -> generateReport (curDir++"/"++a) (a++b)) gpxFiles

-- | Creates empty directory for each web report
createEmptyDir :: FilePath -> IO ()
createEmptyDir dir = do
       exists <- doesDirectoryExist dir
       (if exists then removeDirectoryRecursive dir >> createDirectory dir else createDirectory dir)

-- | Generates the HTML report for each .gpx file
generateReport :: FilePath -> FilePath -> IO ()
generateReport webDir gpxFile  = do
         points <- readGPX gpxFile
         case length points of
          0 -> putStr "Unable to parse GPX file. Skipping..."
          _ -> do
           createEmptyDir webDir
           putStr "Generating statistical charts...\n"
           renderToPng (chart1 points) (webDir++"/chart1.png")
           renderToPng (chart2 points) (webDir++"/chart2.png")
           writeFile (webDir++"/index.html") $ renderHtml $ generateHtmlPage points
           putStr "Downloading OpenStreetMap tiles...\n"
           generateOsmMap webDir points
           putStr $ "Processing '"++gpxFile++"' complete. Report saved in: "++webDir++"/index.html\n"
           return ()