-- {-# LANGUAGE ScopedTypeVariables #-}

-- | This module utilizes the perl gpx2png program
-- to download the OSM tiles, and the GraphicsMagick bindings
-- to resize the OSM image if necessary
module Gps2HtmlReport.OsmChart where

import System.FilePath
import System.Directory
import System.Process
import Graphics.GD
import Control.Monad

-- | If the generated OSM image has a greater width than 800 pixels, it is scaled to have a width of 800 pixels.
fitToWidth :: FilePath -> IO ()
fitToWidth imgPath = do
   img <- loadPngFile imgPath
   dimensions <- imageSize img
   let width = fst dimensions
   when (width > 800) $ resizeImg imgPath img dimensions

-- | Uses the GraphicsMagick bindings the resize the image
resizeImg :: FilePath -> Image -> (Int,Int) -> IO ()  
resizeImg imgPath img dimensions = do
   let resizeRatio = fromIntegral (fst dimensions) / 800.0
       height = round (fromIntegral (snd dimensions) / resizeRatio)
   resizedImg <- resizeImage 800 height img
   savePngFile imgPath resizedImg
       
-- | Calls the perl `gpx2png' utility to download the relevant OSM tiles
createOsmMap :: [Char] -> [Char] -> IO [()]
createOsmMap webDir gpxFile = do
   (_,_,_,pid) <- createProcess (shell ("perl gpx2png/gpx2png.pl -q -o "++webDir++"/osm.png "++ gpxFile))
   waitForProcess pid
   fitToWidth $ webDir++"/osm.png"
   curDir <- getCurrentDirectory
   allFiles <- getDirectoryContents curDir
   let allFilesSplit = map splitExtension allFiles
   let tmpPngFiles = filter (\(a,b) -> b==".png") allFilesSplit
   mapM (\(a,b) -> removeFile (a++b) ) tmpPngFiles