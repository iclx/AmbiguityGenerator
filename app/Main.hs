module Main where

import Ambiguity
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (beside)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Grid
import System.Random
import System.Environment
import Control.Monad
import Data.List


main :: IO ()
main
 = do [runsStr, samplesStr, rangeStr, filepath] <- getArgs

      let runs = read runsStr
      let samples = read samplesStr
      let range = read rangeStr

      let imageSize = (fromIntegral (800 * 2), fromIntegral (300 * runs))

      ambiguityPlots <- plotAmbiguity runs samples range
      renderableToFile (fo_size .~ imageSize $ fo_format .~ SVG $ def) filepath ambiguityPlots

      return ()


plotContinuous :: Int -> Int -> IO (Renderable (LayoutPick Double Double Double))
plotContinuous runs samples
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do gen <- mkContinuousState (1 / fromIntegral samples) 0.0001 0 0
                  values <- generateContinuous gen samples
                  return $ plotValues (0, 1) values


plotBits :: Int -> Int -> IO (Renderable (LayoutPick Double Double Double))
plotBits runs samples
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do gen <- mkContinuousState (1 / fromIntegral samples) 0.0001 0 0
                  values <- fmap fromIntegral <$> generateBits gen samples
                  return $ plotValues (0, 1) values


plotShuffleAmbiguity :: Int -> Int -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotShuffleAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do ambi <- mkAmbiGen (1 / fromIntegral samples) (0.0001) 0 0
                  values <- generate ambi (4 * samples)
                  shuffled <- randomShuffle values

                  return $ combinedPlot range (take samples shuffled)


plotAmbiguity :: Int -> Int -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do ambi <- mkAmbiGen (1 / fromIntegral samples) (0.0001) 0 0
                  values <- generate ambi samples
                  return $ combinedPlot range values


combinedPlot :: Integer -> [AmbiGenReal] -> Grid (Renderable (LayoutPick Double Double Double))
combinedPlot range values
  = histRealizations `beside` line `beside` histBinary `beside` hist
    where
      hist = layoutToGrid $ plotHistogram (Just (0, fromInteger range)) samples
        where samples = map (fromIntegral . toRange (1, range)) values

      histBinary = layoutToGrid $ plotHistogram (Just (0, 1)) samples
        where samples = map (fromIntegral . toRange (0, 1)) values

      histRealizations = layoutToGrid $ plotHistogram Nothing values

      line = layoutToGrid $ plotRealizations (map realToFrac values)


plotValues :: (Double, Double) -> [Double] -> Grid (Renderable (LayoutPick Double Double Double))
plotValues range values
  = line `beside` histRealizations
    where
      histRealizations = layoutToGrid $ plotHistogram (Just range) values
      line = layoutToGrid $ plotRealizations (map realToFrac values)


plotHistogram :: Maybe (Double, Double) -> [Double] -> Layout Double Double
plotHistogram range samples
  = layout_plots .~ [hist] $ def
  where
    hist = histToPlot $
           plot_hist_values .~ samples $
           plot_hist_range .~ range $
           defaultNormedPlotHist


plotRealizations :: [Double] -> Layout Double Double
plotRealizations realizations
  = layout_plots .~ [line] $ def
  where
    line = toPlot $
           plot_lines_style .~ solidLine 0.25 (opaque green) $
           plot_lines_values .~ [zip [1..] realizations] $
           def
