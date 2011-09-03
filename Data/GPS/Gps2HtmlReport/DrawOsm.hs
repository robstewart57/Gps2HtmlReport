
module Data.GPS.Gps2HtmlReport.DrawOsm where

import Prelude
import Data.GPS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Graphics.Transform.Magick.Types hiding (Image, minimum, maximum)
import Network.Curl.Download
import Data.Bits
import Graphics.GD
import Data.Maybe

import Data.GPS.Gps2HtmlReport.JourneyStats

baseurl = "http://tile.openstreetmap.org"
tilesprefix = "tile"
tilesourcename = "osmrender"
zoom = 16::Int

data TileCoords = TileCoords { minX :: Int
                     , maxX :: Int  
                     , minY :: Int  
                     , maxY :: Int 
                     } 


tileNumbers :: Double -> Double -> [(Int,Int)]
tileNumbers latitude longitude = 
             let xtile = ((longitude+180) / 360) * fromInteger (shift (1::Integer) zoom)
                 tmp = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = ((1-tmp / pi) / 2.0) * fromInteger (shift (1::Integer) zoom)
                 bounds x = [ceiling x, floor x]
             in [(xt,yt) | xt <- bounds xtile, yt <- bounds ytile]

maxTile :: [(Int,Int)] -> (Int,Int)
maxTile [] = error "There is no max tile of an empty list"
maxTile (x:xs) = go x xs
  where
    go a [] = a
    go a (y:ys) = if fst y >= fst a && snd y >= snd a then go y ys else go a ys

secant a = 1 / cos a

initCoords = TileCoords {minX = 1000000, maxX = -1000, minY = 1000000, maxY = -1000}

-- | Determines the minimum and maximum of the X and Y tiles
-- to be downloaded from OSM
determineTiles :: [WptType] -> TileCoords -> TileCoords
determineTiles [] _ = initCoords
determineTiles [wpt] tCoords =
       let curMinX = minX tCoords
           curMaxX = maxX tCoords
           curMinY = minY tCoords
           curMaxY = maxY tCoords
           tiles = tileNumbers (value (lat wpt)) (value (lon wpt))
           newMaxX = maximum $ curMaxX : (map fst tiles)
           newMinX = minimum $ curMinX : (map fst tiles)
           newMaxY = maximum $ curMaxY : (map snd tiles)
           newMinY = minimum $ curMinY : (map snd tiles) 
       in tCoords {minX = newMinX, maxX = newMaxX, minY = newMinY, maxY = newMaxY}

determineTiles (wpt:wpts) tCoords = 
       let tileScope = determineTiles [wpt] tCoords
       in determineTiles wpts tileScope

deltaLat :: TileCoords -> Int
deltaLat tCoords = maxX tCoords - minX tCoords

deltaLong :: TileCoords -> Int
deltaLong tCoords = maxY tCoords - minY tCoords

-- | Takes the boundaries of the OSM tiles, and generates
-- [(Int,Int)] containing a list of all OSM tiles that
-- need downloading
selectedTiles :: TileCoords -> [(Int,Int)]
selectedTiles tCoords = 
         let minx = minX tCoords
             maxx = maxX tCoords
             miny = minY tCoords
             maxy = maxY tCoords
         in [(i,j) | i <- [minx..maxx], j <- [miny..maxy]]
       
-- | Formats the filename string
filenameStr xTile yTile = tilesprefix ++ tilesourcename ++ "-z"++show zoom++"-x"++show xTile++"-y"++show yTile++".png"

-- | Formats the URL string
urlStr xTile yTile = baseurl ++"/"++show zoom++"/"++show xTile++"/"++show yTile++".png"

rectangle x' y' = Rectangle {width=256, height=256, x = x'*256, y = y'*256}

-- | Takes the URL of a given OSM tile and uses curl to download it
downloadFile :: String -> IO Image
downloadFile url = do
  response <- openURI url
  case response of
    Left err  -> error err
    Right img -> loadPngByteString img

-- | Takes the boundaries of the OSM tiles covering the
-- the 'Trail', uses 'placeTile' to download the tile
-- and to place each tile on the background layer
makeOSMLayer :: TileCoords -> IO Image
makeOSMLayer tCoords = do
        backgroundImg <- newImage (((maxX tCoords - minX tCoords)+1)*256,((maxY tCoords - minY tCoords)+1)*256)
        mapM_ (\(a,b) -> placeTile a b backgroundImg tCoords) (selectedTiles tCoords)
        return backgroundImg

-- | Used to create a mosaic of all downloaded OSM tiles to generate
-- the background layer for plotting the @Trail@ onto the @Image@
placeTile x y backgroundImg tCoords = do
          img <- downloadFile $ urlStr x y
          copyRegion (0,0) (256,256) img (256*(x-minX tCoords),256*(y-minY tCoords)) backgroundImg

projectMercToLat rely = (180 / pi) * atan (sinh rely)

-- | Used by @pixelPosForCoord@ for N,S,E,W coordinates for (x,y) values
project :: Int -> Int -> (Double,Double,Double,Double)
project x y = 
  let unit = 1.0 / (2.0 ** fromIntegral zoom)
      rely1 = fromIntegral y * unit
      rely2 = rely1 + unit
      limity = pi
      rangey = 2.0 * limity
      rely1' = limity - rangey * rely1
      rely2' = limity - rangey * rely2
      lat1 = projectMercToLat rely1'
      lat2 = projectMercToLat rely2'
      unit' = 360.0 / (2.0 ** fromIntegral zoom)
      long1 = (-180.0) + fromIntegral x * unit'
  in (lat2,long1,lat1,long1+unit') -- S,W,N,E
  
-- | Takes a WptType, and the OSM tile boundaries
-- and generates (x,y) points to be placed on the @Image@
pixelPosForCoord [] _ = (0,0)
pixelPosForCoord [wpt] tCoord =
             let lat' = value $ lat wpt
                 lon' = value $ lon wpt
                 tile = maxTile $ tileNumbers lat' lon'
                 xoffset = (fst tile - minX tCoord) * 256
                 yoffset = (snd tile - minY tCoord) * 256
                 (south,west,north,east) = (uncurry project tile)
                 x = round $ (lon' - west) * 256.0 / (east - west) + fromIntegral xoffset
                 y = round $ (lat' - north) * 256.0 / (south - north) + fromIntegral yoffset
             in (x,y)

-- | Takes the 'WptType' and draws lines between every
-- point to point connection in the 'Trail'
drawLines :: [WptType] -> TileCoords -> Image -> IO Image
drawLines [] _ img = return img
drawLines [_] _ img = return img
drawLines (wpt:wpts) tCoord img = do
       let start = pixelPosForCoord [wpt] tCoord
           end = pixelPosForCoord [head wpts] tCoord
           minEle = fromMaybe 0 $ fmap snd $ findPoint wpts wpt ele (<)
           maxEle = fromMaybe 0 $ fmap snd $ findPoint wpts wpt ele (>)
       drawLine' start end img (minEle,fromMaybe 0 $ ele wpt,maxEle) 0
       drawLines wpts tCoord img

-- | This is a fix on the fact that the 'drawLine' function
-- provided by the GD bindings do not provid a `width' parameter
drawLine' :: Point -> Point -> Image -> (Double,Double,Double) -> Int -> IO ()
drawLine' start end img (minEle,ele',maxEle) i
      | i < 6 = drawLine (fst start+(i-3),snd start-(i-3)) (fst end+(i-3),snd end-(i-3)) color' img >> drawLine' start end img (minEle,ele',maxEle) (i+1)
      | otherwise = return ()
     where range = maxEle - minEle
           x' = ele' - minEle
           x'' = x' / range
           color' = lineColor $ round $ 255.0*x''

-- | Uses a sliding scale for the red value in the RGB Color
-- to show a sliding color from green to yellow in accordance
-- with the relative elevation of a given WptType in the Trail
lineColor redVal = rgb redVal 255 0

-- | Adds the copyright text in accordance with
-- http://wiki.openstreetmap.org/wiki/Legal_FAQ
addCopyright img = do
         size <- imageSize img
         let copyrightS = "Tile images © OpenStreetMap (and) contributors, CC-BY-SA"
             pos = (10,snd size-10)
             black = rgb 0 0 0
         useFontConfig True
         drawString "monospace" 6.0 0.0 pos copyrightS black img

-- | If the generated OSM image has a greater width than 800 pixels, it is scaled to have a width of 800 pixels.
fitToWidth :: Image -> IO Image
fitToWidth img = do
   dimensions <- imageSize img
   let width = fst dimensions
   if width > 800 then resizeImg img dimensions else return img

-- | Uses the GraphicsMagick bindings the resize the image
resizeImg :: Image -> (Int,Int) -> IO Image  
resizeImg img dimensions = do
   let resizeRatio = fromIntegral (fst dimensions) / 800.0
       height = round (fromIntegral (snd dimensions) / resizeRatio)
   resizeImage 800 height img

-- | Takes the destination directory for the web content,
-- the (Trail WptType), and uses the DrawOsm functions
-- to generate an `osm.png' file showing the trail.
generateOsmMap webDir points = do
  let tiles = determineTiles points initCoords
  backgroundImg <- makeOSMLayer tiles
  tmpImg <- imageSize backgroundImg
  imgWithLines <- drawLines points tiles backgroundImg
  resizedImg <- fitToWidth imgWithLines
  addCopyright resizedImg
  savePngFile (webDir++"/osm.png") resizedImg
