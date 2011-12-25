-- | This module uses the JourneyStats module to generate
-- the statistics about the journey WayPoints, then
-- uses the Cairo bindings to generate the charts
module Data.GPS.Gps2HtmlReport.JourneyCharts (
  renderToPng, -- :: (t, OutputType -> Renderable a) -> FilePath -> IO (PickFn a)
  chart1, -- :: [WptType] -> (String, OutputType -> Renderable ())
  chart2 -- :: [WptType] -> (String, OutputType -> Renderable ())
  )where

import Data.GPS
import Data.Maybe
import Graphics.Rendering.Chart
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Data.Time.LocalTime

import Data.GPS.Gps2HtmlReport.JourneyStats (ptsElevation,avrSpeedOverTime,accumDistance,findPoint)

data OutputType = Window | PNG | PS | PDF | SVG

chooseLineWidth Window = 1.0
chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25
chooseLineWidth SVG = 0.25

-- | Generates the Cairo chart showing speed and elevation over time
speedAndElevationOverTimeChart :: [WptType] -> OutputType -> Renderable ()
speedAndElevationOverTimeChart points otype = toRenderable layout
  where
    layout = layout1_title ^="Speed, Average Speed & Elevation"
           $ layout1_title_style ^= defaultFontStyle { font_size_ = 12.0 }
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_right_axis ^= defaultLayoutAxis { laxis_title_ = "Elevation (metres)", laxis_override_ = axisGridHide }
           $ layout1_left_axis ^: laxis_title ^= "Speed (metres)"
 	   $ layout1_plots ^= [ Right (toPlot elevationArea),
                                Left (toPlot speedLine),
                                Left (toPlot avrSpeedLine)  
                              ]
           $ setLayout1Foreground fg
           defaultLayout1
   

    lineStyle c = line_width ^= 1 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    theSpeeds = [(utcToLocalTime utc theTime,spd) | (theTime,spd) <- avgSpeeds 10 points]

    speedLine = plot_lines_style ^= lineStyle (opaque blue)
           $ plot_lines_values ^= [[ (theTime,speed) | (theTime,speed) <- theSpeeds]]
           $ plot_lines_title ^= "Speed"
           $ defaultPlotLines

    avrSpeedLine = plot_lines_style ^= lineStyle (red `withOpacity` 0.5)
           $ plot_lines_values ^= [[ (theTime,speed) | (theTime,speed) <- avrSpeedOverTime theSpeeds 0.0 0.0 []]]
           $ plot_lines_title ^= "Avr Speed"
           $ defaultPlotLines 

    elevationArea = plot_fillbetween_style ^= solidFillStyle (green `withOpacity` 0.1)
           $ plot_fillbetween_values ^= [ (theTime,(0,elevation)) | (theTime,elevation) <- ptsElevation points]
           $ plot_fillbetween_title ^= "Elevation"
           $ defaultPlotFillBetween

    fg = opaque black


-- |  Generates the Cairo chart showing accumulative distance and elevation over time, with spots showing maximum and minimum elevation points
accumDistanceAndElevationChart :: [WptType] -> OutputType -> Renderable ()
accumDistanceAndElevationChart points otype = toRenderable layout
  where
    layout = layout1_title ^="Accumulative Distance & Elevation"
           $ layout1_title_style ^= defaultFontStyle { font_size_ = 12.0 }
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_right_axis ^= defaultLayoutAxis { laxis_title_ = "Distance (metres)", laxis_override_ = axisGridHide }
           $ layout1_left_axis ^: laxis_title ^= "Elevation (metres)"
           $ layout1_plots ^= [ Right (toPlot accumDistanceArea),
                                Left (toPlot elevationLine),
                                Left (toPlot spots)
                              ]
           $ setLayout1Foreground fg
           defaultLayout1

    lineStyle c = line_width ^= 1 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    elevationLine = plot_lines_style ^= lineStyle (opaque black)
           $ plot_lines_values ^= [[ (theTime,elevation) | (theTime,elevation) <- ptsElevation points]]
           $ plot_lines_title ^= "Elevation"
           $ defaultPlotLines 

    accumDistanceArea = plot_fillbetween_style ^= solidFillStyle (red `withOpacity` 0.2)
           $ plot_fillbetween_values ^= [ (theTime,(0,accumDist)) | (theTime,accumDist) <- accumDistance points 0.0]
           $ plot_fillbetween_title ^= "Distance"
           $ defaultPlotFillBetween

    spotMaxPoint = let point = findPoint points (head points) ele (>) 
                   in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

    spotMinPoint = let point = findPoint points (head points) ele (<)
                   in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

    spots = area_spots_title ^= "Altitude"
          $ area_spots_max_radius ^= 5
          $ area_spots_values ^= (if  isJust spotMinPoint && isJust spotMaxPoint then [fromJust spotMinPoint,fromJust spotMaxPoint] else [])
          $ defaultAreaSpots

    fg = opaque black

renderToPng :: (t, OutputType -> Renderable a) -> FilePath -> IO (PickFn a)
renderToPng (_,ir) = renderableToPNGFile (ir Window) 384 288

chart1 :: [WptType] -> (String, OutputType -> Renderable ())
chart1 points = ("speedAndElevationOverTimeChart", speedAndElevationOverTimeChart points)

chart2 :: [WptType] -> (String, OutputType -> Renderable ())
chart2 points = ("accumDistanceAndElevationChart", accumDistanceAndElevationChart points)

