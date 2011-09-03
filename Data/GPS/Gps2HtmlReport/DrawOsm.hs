{-# LANGUAGE ScopedTypeVariables  #-}

module Data.GPS.Gps2HtmlReport.DrawOsm where

import Prelude
import Data.GPS
import Data.ByteString.Char8 hiding (head)
import Graphics.Transform.Magick.Types hiding (Image)
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

tileNumber :: Double -> Double -> (Int,Int)
tileNumber latitude longitude = 
             let xtile = round $ ((longitude+180) / 360) * fromInteger (shift (1::Integer) zoom)
                 tmp::Double = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = round $ ((1-tmp / pi) / 2.0) * fromInteger (shift (1::Integer) zoom)
             in (xtile,ytile)

secant a = 1 / cos a

initCoords = TileCoords {minX = 1000000, maxX = 0, minY = 1000000, maxY = 0}

determineTiles :: [WptType] -> TileCoords -> TileCoords
determineTiles [] _ = initCoords
determineTiles [wpt] tCoords =
       let curMinX = minX tCoords
           curMaxX = maxX tCoords
           curMinY = minY tCoords
           curMaxY = maxY tCoords
           tile = tileNumber (value (lat wpt)) (value (lon wpt))
           newMaxX = max curMaxX (fst tile)
           newMinX = min curMinX (fst tile)
           newMaxY = max curMaxY (snd tile)
           newMinY = min curMinY (snd tile) 
       in tCoords {minX = newMinX, maxX = newMaxX, minY = newMinY, maxY = newMaxY}

determineTiles (wpt:wpts) tCoords = 
       let tileScope = determineTiles [wpt] tCoords
       in determineTiles wpts tileScope

deltaLat :: TileCoords -> Int
deltaLat tCoords = maxX tCoords - minX tCoords

deltaLong :: TileCoords -> Int
deltaLong tCoords = maxY tCoords - minY tCoords

selectedTiles :: TileCoords -> [(Int,Int)]
selectedTiles tCoords = 
         let minx = minX tCoords
             maxx = maxX tCoords
             miny = minY tCoords
             maxy = maxY tCoords
         in [(i,j) | i <- [minx..maxx], j <- [miny..maxy]]
       
filenameStr xTile yTile = tilesprefix ++ tilesourcename ++ "-z"++show zoom++"-x"++show xTile++"-y"++show yTile++".png"

urlStr xTile yTile = baseurl ++"/"++show zoom++"/"++show xTile++"/"++show yTile++".png"

rectangle x' y' = Rectangle {width=256, height=256, x = x'*256, y = y'*256}

downloadFile :: String -> IO Image
downloadFile url = do
  response <- openURI url
  case response of
    Left err  -> error err
    Right img -> loadPngByteString img

makeOSMLayer :: TileCoords -> IO Image
makeOSMLayer tCoords = do
        backgroundImg <- newImage ((maxX tCoords - minX tCoords)*256,(maxY tCoords - minY tCoords)*256)
        mapM_ (\(a,b) -> placeTile a b backgroundImg tCoords) (selectedTiles tCoords)
        return backgroundImg

placeTile x y backgroundImg tCoords = do
          img <- downloadFile $ urlStr x y
          copyRegion (0,0) (256,256) img (256*(x-minX tCoords),256*(y-minY tCoords)) backgroundImg

projectMercToLat rely = (180 / pi) * atan (sinh rely)

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
  
pixelPosForCoord [] _ = (0,0)
pixelPosForCoord [wpt] tCoord =
             let lat' = value $ lat wpt
                 lon' = value $ lon wpt
                 tile = tileNumber lat' lon'
                 xoffset = (fst tile - minX tCoord) * 256
                 yoffset = (snd tile - minY tCoord) * 256
                 (south,west,north,east) = (uncurry project tile)
                 x = round $ (lon' - west) * 256.0 / (east - west) + fromIntegral xoffset
                 y = round $ (lat' - north) * 256.0 / (south - north) + fromIntegral yoffset
             in (x,y)

drawLines :: [WptType] -> TileCoords -> Image -> IO Image
drawLines [] _ img = return img
drawLines [_] _ img = return img
drawLines (wpt:wpts) tCoord img = do
       let start = pixelPosForCoord [wpt] tCoord
           end = pixelPosForCoord [head wpts] tCoord
           minEle = snd $ fromJust $ findPoint wpts wpt ele (<)
           maxEle = snd $ fromJust $ findPoint wpts wpt ele (>)
       drawLine' start end img (minEle,fromJust $ ele wpt,maxEle) 0
       drawLines wpts tCoord img

-- | This is a fix on the fact that the @drawLine@ function
-- provided by the GD bindings do not provid a `width' parameter
drawLine' :: Point -> Point -> Image -> (Double,Double,Double) -> Int -> IO ()
drawLine' start end img (minEle,ele',maxEle) i
      | i < 6 = drawLine (fst start+(i-3),snd start-(i-3)) (fst end+(i-3),snd end-(i-3)) color' img >> drawLine' start end img (minEle,ele',maxEle) (i+1)
      | otherwise = return ()
     where range = maxEle - minEle
           x' = ele' - minEle
           x'' = x' / range
           color' = lineColor $ round $ 255.0*x''


lineColor redVal = rgb redVal 255 0

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

generateOsmMap webDir points = do
  let tiles = determineTiles points initCoords
  backgroundImg <- makeOSMLayer tiles
  imgWithLines <- drawLines points tiles backgroundImg
  resizedImg <- fitToWidth imgWithLines
  savePngFile (webDir++"/osm.png") resizedImg
