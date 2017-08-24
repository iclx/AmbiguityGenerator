module Main where

import Ambiguity
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (beside)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Grid
import System.Random


main :: IO ()
main
 = do source <- newStdGen
      let samples = 100
      let range = 10
      let values = generate (mkAmbiGen source (0.0001) (0.0001)) samples

      toFile def ("realizations.png") (plot (line "realizations" [zip ([1..] :: [Integer]) values]))


combinedPlot :: Integer -> [Double] -> Renderable ()
combinedPlot range values
  = toRenderable grid
    where
      grid = tval hist `beside` tval hist

      hist = plotHistogram range samples
        where samples = map (fromIntegral . toRange (1, range)) values

      line = plotRealizations values

plotHistogram :: Integer -> [Double] -> Layout Double Double -- Plot Double Double
plotHistogram range samples
  = layout
  where
    layout = layout_plots .~ [hist] $ def

    hist = histToPlot $
           plot_hist_bins .~ fromIntegral range $
           plot_hist_values .~ samples $
           defaultNormedPlotHist

plotRealizations :: [Double] -> Plot Double Double
plotRealizations realizations
  = toPlot $
    plot_lines_style .~ solidLine 0.25 (opaque green) $
    plot_lines_values .~ [zip [1..] realizations] $
    def
    

{-
toRenderable layout
    where layout
            = layout_plots .~ [ histToPlot hist ] $
              def :: Layout PlotIndex Double

          hist
            = plot_hist_values .= hist $
              plot_hist_bins .= range $
              defaultNormedPlotHist
            
-}
