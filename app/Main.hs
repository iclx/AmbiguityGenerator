module Main where

import Ambiguity
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (beside)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Grid
import System.Random
import System.Environment
import Control.Monad


main :: IO ()
main
 = do [runsStr, samplesStr, rangeStr, filepath] <- getArgs

      let runs = read runsStr
      let samples = read samplesStr
      let range = read rangeStr

      let imageSize = (800, 300 * runs)

      ambiguityPlots <- plotAmbiguity runs samples range
      renderableToFile (fo_size .~ imageSize $ def) filepath ambiguityPlots

      return ()


plotAmbiguity :: Int -> Integer -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do source <- newStdGen
                  let values = generate (mkAmbiGen source (1 / fromIntegral samples) (0.0001)) samples
                  return $ combinedPlot range values


combinedPlot :: Integer -> [AmbiGenReal] -> Grid (Renderable (LayoutPick Double Double Double))
combinedPlot range values
  = line `beside` hist
    where
      hist = layoutToGrid $ plotHistogram range samples
        where samples = map (fromIntegral . toRange (1, range)) values

      line = layoutToGrid $ plotRealizations (map realToFrac values)


plotHistogram :: Integer -> [Double] -> Layout Double Double
plotHistogram range samples
  = layout_plots .~ [hist] $ def
  where
    hist = histToPlot $
           plot_hist_bins .~ fromIntegral range $
           plot_hist_values .~ samples $
           defaultNormedPlotHist


plotRealizations :: [Double] -> Layout Double Double
plotRealizations realizations
  = layout_plots .~ [line] $ def
  where
    line = toPlot $
           plot_lines_style .~ solidLine 0.25 (opaque green) $
           plot_lines_values .~ [zip [1..] realizations] $
           def
