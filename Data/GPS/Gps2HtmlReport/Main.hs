
{-# LANGUAGE RecordWildCards,DeriveDataTypeable,DoAndIfThenElse  #-}

module Main where

import Data.GPS hiding (name, id)
import System.FilePath
import System.Directory
import Text.Html hiding (name)
import System.Console.CmdArgs
import Codec.Archive.Tar
import System.Random

import Data.GPS.Gps2HtmlReport.HTMLGenerator (generateHtmlPage)
import Data.GPS.Gps2HtmlReport.JourneyCharts (renderToPng,chart1,chart2)
import Data.GPS.Gps2HtmlReport.DrawOsm (generateOsmMap)

data MyOptions = MyOptions
    { imageonly :: Bool,
      archive :: Bool,
      hashnames :: Bool
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
myProgOpts :: MyOptions
myProgOpts = MyOptions
    { imageonly = def &= help "Generates only an image of the track overlay on an OpenStreetMap layer",
      archive = def &= help "Produce tar archive for web and image files",
      hashnames = def &= help "Create reports in hashed directory names"
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
_PROGRAM_VERSION = "0.3"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A Haskell utility to generate HTML page reports of GPS Tracks and Track overlays on OpenStreetMap tiles"
_COPYRIGHT = "(C) Rob Stewart 2011"

main :: IO [()]
main = do
    opts <- getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: MyOptions -> IO [()]
optionHandler opts@MyOptions{..}  = exec opts

exec :: MyOptions -> IO [()]
exec MyOptions{..} =
  processGps imageonly archive hashnames

-- | Reads the current directory for all .gpx files, then maps to `generateReport' for each one
processGps :: Bool -> Bool -> Bool -> IO [()]
processGps imageonly archive hashnames = do
   curDir <- getCurrentDirectory
   allFiles <- getDirectoryContents curDir
   let allFilesSplit = map splitExtension allFiles
   let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
   putStrLn ("Processing "++show (length gpxFiles)++" file(s)...")
   mapM (\(a,b) -> generateReport curDir a (a++b) imageonly archive hashnames) gpxFiles

-- | Creates empty directory for each web report
createEmptyDir :: FilePath -> IO ()
createEmptyDir dir = do
       exists <- doesDirectoryExist dir
       if exists then removeDirectoryRecursive dir >> createDirectory dir else createDirectory dir

-- | Generates the HTML report for each .gpx file,
-- or simply an osm.png file if the '--imageonly' argument
-- is used
generateReport :: FilePath -> String -> FilePath -> Bool -> Bool -> Bool -> IO ()
generateReport webDir projectName gpxFile imageonly archive hashnames = do
 points <- readGPX gpxFile

 -- A successfully parsed gpx file will create
 -- a list of waypoints
 case length points of
  0 -> putStrLn "Unable to parse GPX file. Skipping..."
  _ -> do
   let projName = if hashnames then do
                hashedProjName
              else return projectName
   name <- projName
   let path = (webDir++"/"++name)
   createEmptyDir path
   
   -- If --imageonly was set, only produce OSM image
   if imageonly then do
    putStrLn "Downloading OpenStreetMap tiles..."
    generateOsmMap path points
    putStrLn $ gpxFile ++" processed. Image saved in: "++path++"/osm.png"

   -- Otherwise, create the complete web page report
   else do
    putStrLn "Generating statistical charts..." 
    renderToPng (chart1 points) (path++"/chart1.png")
    renderToPng (chart2 points) (path++"/chart2.png")
    writeFile (path++"/index.html") (renderHtml $ generateHtmlPage points)
    putStrLn "Downloading OpenStreetMap tiles..."
    generateOsmMap path points
    putStrLn $ gpxFile ++" processed. Report saved in: "++path++"/index.html"
   
   -- Lastly, if --archive was set, then create .tar
   -- and put all contents into this archive
   if archive then do
    putStrLn $ "Creating archive: " ++ (path++".tar")
    create (path++".tar") webDir [name]
   else return ()
               

hashedProjName = do
  r <- randomIO
  if r > 0 then return (show (r::Int)) else hashedProjName