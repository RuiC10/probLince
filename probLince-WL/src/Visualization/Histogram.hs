{-# LANGUAGE DeriveGeneric #-}

module Visualization.Histogram where

import Data.Colour.SRGB
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import qualified Data.Aeson as A

data HistConfig = H { imageSize :: (Int, Int)
                    , fileName :: String
                    , title :: String
                    , range :: Maybe (Double, Double)
                    , showZeroBins :: Bool
                    , numBins :: Int
                    , x_axis_title :: String
                    , y_axis_title :: String
                    , var :: String
                    , varColor :: (Double, Double, Double)
                    , y_axisSize :: Double
                    , x_axisSize :: Double
                    , y_labelSize :: Double
                    , x_labelSize :: Double
                    , titleSize :: Double
                    , legendSize :: Double
                    } deriving (Show, Read, Generic)

instance A.FromJSON HistConfig

makeHist :: HistConfig -> [Double] -> Plot Double Int
makeHist hist values = histToPlot
                       $ plot_hist_bins .~ numBins hist
                       $ plot_hist_title .~ (var hist)
                       $ plot_hist_values .~ values 
                       $ plot_hist_drop_lines .~ True
                       $ plot_hist_no_zeros .~ not (showZeroBins hist)
                       $ plot_hist_fill_style . fill_color .~ (opaque $ sRGB r g b)
                       $ plot_hist_line_style . line_color .~ (opaque $ sRGB r g b)
                       $ plot_hist_range .~ range hist
                       $ (def :: PlotHist Double Int)
    where
        (r,g,b) = varColor hist

plotHist :: [Plot Double Int] -> HistConfig -> IO()
plotHist plots histConf = toFile (fo_size .~ (imageSize histConf) $ def) (fileName histConf) $ do
                            layout_plots .= plots 
                            layout_title .= title histConf
                            layout_title_style . font_size .= (titleSize histConf)
                            layout_x_axis . laxis_title .= x_axis_title histConf
                            layout_x_axis . laxis_title_style . font_size .= (x_axisSize histConf)
                            layout_y_axis . laxis_title .= y_axis_title histConf
                            layout_y_axis . laxis_title_style . font_size .= (y_axisSize histConf)
                            layout_y_axis . laxis_style . axis_label_style . font_size .= (x_labelSize histConf)
                            layout_x_axis . laxis_style . axis_label_style . font_size .= (y_labelSize histConf)
                            layout_legend .= (Just $ legend_label_style . font_size .~ (legendSize histConf) $ def)

timeHist :: HistConfig -> [Double] -> IO()
timeHist hist stats = plotHist [makeHist hist stats] hist

varHist :: HistConfig -> Map String [Double] -> IO()
varHist hist stats = plotHist [makeHist hist (stats Map.! (var hist))] hist
