
{-# LANGUAGE RecordWildCards,DeriveDataTypeable  #-}

module Main where

import Data.GPS hiding (name, id)
import System.FilePath
import System.Directory
import Text.Html hiding (name)
import System.Console.CmdArgs

import Data.GPS.Gps2HtmlReport.HTMLGenerator (generateHtmlPage)
import Data.GPS.Gps2HtmlReport.JourneyCharts (renderToPng,chart1,chart2)
import Data.GPS.Gps2HtmlReport.DrawOsm (generateOsmMap)

data MyOptions = MyOptions
    { imageOnly :: Bool
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    { imageOnly = def &= help "Generates only an image of the track overlay on an OpenStreetMap layer"
    }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "gps2HtmlReport"
_PROGRAM_VERSION = "0.2.3"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A Haskell utility to generate HTML page reports of GPS Tracks and Track overlays on OpenStreetMap tiles"
_COPYRIGHT = "(C) Rob Stewart 2011"

main :: IO [()]
main = do
    opts <- getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: MyOptions -> IO [()]
optionHandler opts@MyOptions{..}  =
    exec opts

exec :: MyOptions -> IO [()]
exec MyOptions{..} =
    if imageOnly
            then processGps False
            else processGps True
    

-- | Reads the current directory for all .gpx files, then maps to `generateReport' for each one
processGps :: Bool -> IO [()]
processGps fullReport = do
   curDir <- getCurrentDirectory
   allFiles <- getDirectoryContents curDir
   let allFilesSplit = map splitExtension allFiles
   let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
   putStr ("Processing "++show (length gpxFiles)++" file(s)...\n")
   mapM (\(a,b) -> generateReport (curDir++"/"++a) (a++b) fullReport) gpxFiles

-- | Creates empty directory for each web report
createEmptyDir :: FilePath -> IO ()
createEmptyDir dir = do
       exists <- doesDirectoryExist dir
       if exists then removeDirectoryRecursive dir >> createDirectory dir else createDirectory dir

-- | Generates the HTML report for each .gpx file,
-- or simply an osm.png file if the '--imageonly' argument
-- is used
generateReport :: FilePath -> FilePath -> Bool -> IO ()
generateReport webDir gpxFile fullReport = do
         points <- readGPX gpxFile
         case length points of
          0 -> putStr "Unable to parse GPX file. Skipping..."
          _ -> do
           createEmptyDir webDir
           if fullReport then do
               putStr "Generating statistical charts...\n" 
               renderToPng (chart1 points) (webDir++"/chart1.png")
               renderToPng (chart2 points) (webDir++"/chart2.png")
               writeFile (webDir++"/index.html") (renderHtml $ generateHtmlPage points)
               putStr "Downloading OpenStreetMap tiles...\n"
               generateOsmMap webDir points
               putStr $ "Processing '"++gpxFile++"' complete. Report saved in: "++webDir++"/index.html\n"
              else do
               putStr "Downloading OpenStreetMap tiles...\n"
               generateOsmMap webDir points
               putStr $ "Processing '"++gpxFile++"' complete. Image saved in: "++webDir++"/osm.png\n"