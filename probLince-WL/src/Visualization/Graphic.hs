{-# LANGUAGE DeriveGeneric #-}

module Visualization.Graphic where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Util.Cp
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Colour.SRGB
import GHC.Generics
import qualified Data.Aeson as A

data GraphicConfig = G { imageSize :: (Int, Int)
                       , fileName :: String
                       , title :: String
                       , vars :: [(String, (Double, Double, Double))]
                       , axisTitle :: (String, String)
                       , y_axisSize :: Double
                       , x_axisSize :: Double
                       , y_labelSize :: Double
                       , x_labelSize :: Double
                       , titleSize :: Double
                       , legendSize :: Double
                       } deriving (Show, Read, Generic)

instance A.FromJSON GraphicConfig 

discontinuityPoints :: [[(Int, Double)]] -> [(Int, Double)]
discontinuityPoints [] = [] 
discontinuityPoints [h] = [last h]
discontinuityPoints (h:y:t) | last h /= head y = last h : head y : discontinuityPoints (y:t) 
                            | otherwise = discontinuityPoints (y:t) 

plotDiscontinuityPoints :: (Double, Double, Double) -> [(Int, Double)] -> Plot Double Double
plotDiscontinuityPoints (r,g,b) pts = toPlot
                                       $ plot_points_title .~ ""
                                       $ plot_points_values .~ Prelude.map ((/1000000.0) . fromIntegral >< id) pts
                                       $ plot_points_style . point_color .~ (opaque white)
                                       $ plot_points_style . point_border_width .~ 1.5
                                       $ plot_points_style . point_border_color .~ (opaque $ sRGB r g b)
                                       $ plot_points_style . point_radius .~ 3.0
                                       $ def

updateTitle :: String -> [PlotLines Double Double] -> [PlotLines Double Double]
updateTitle _ [] = []
updateTitle var (x:xs) = (plot_lines_title .~ var $ x):xs

plotTrajectory :: GraphicConfig -> (String, [[[(Int, Double)]]]) -> Plot Double Double
plotTrajectory config (var, vals) = let plotVarLine v = plot_lines_values .~ Prelude.map (Prelude.map ((/1000000.0) . fromIntegral >< id)) v
                                                      $ plot_lines_title .~ ""
                                                      $ plot_lines_style . line_color .~ (opaque $ sRGB r g b)
                                                      $ plot_lines_style . line_width .~ 2.0
                                                      $ def 
                                   in foldr joinPlot (toPlot $ (def :: PlotLines Double Double)) 
                                      . uncurry (++)
                                      . split (map (plotDiscontinuityPoints (r,g,b) . discontinuityPoints)) (map toPlot . updateTitle var . map plotVarLine)
                                      $ vals
    where
        (r,g,b) = (p2 . head . Prelude.filter ((== var) . p1) $ vars config)

plotTrajectories :: GraphicConfig -> [(String, [[[(Int, Double)]]])] -> [Plot Double Double]
plotTrajectories conf trjs = map (plotTrajectory conf) $ Prelude.filter (\(x,_) -> any (\(y,_) -> y == x) (vars conf)) trjs

makeGraph :: GraphicConfig -> [Plot Double Double] -> IO ()
makeGraph conf plots = toFile (fo_size .~ (imageSize conf) $ def) (fileName conf) $ do 
                  layout_title .= title conf
                  layout_plots .= plots
                  layout_title_style . font_size .= (titleSize conf)
                  layout_x_axis . laxis_title .= p1 (axisTitle conf)
                  layout_y_axis . laxis_title .= p2 (axisTitle conf)
                  layout_x_axis . laxis_title_style . font_size .= (x_axisSize conf)
                  layout_y_axis . laxis_title_style . font_size .= (y_axisSize conf)
                  layout_y_axis . laxis_style . axis_label_style . font_size .= (x_labelSize conf)
                  layout_x_axis . laxis_style . axis_label_style . font_size .= (y_labelSize conf)
                  layout_legend .= (Just $ legend_label_style . font_size .~ (legendSize conf) $ def)

makeGraphTrajectories :: GraphicConfig -> [Map String [[(Int, Double)]]] -> IO ()
makeGraphTrajectories conf trajectories = makeGraph conf 
                                        . plotTrajectories conf 
                                        . Map.toList 
                                        . Map.unionsWith (++) 
                                        . Prelude.map (Map.map (:[])) 
                                        $ trajectories
