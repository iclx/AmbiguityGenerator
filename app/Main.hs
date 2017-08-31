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


plotTypeclassAmbiguity :: Int -> Int -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotTypeclassAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do source <- newStdGen
                  let ambi = mkAmbiGen source 0 0
                  let haskellValues = take samples $ randomRs (1 :: Int, 10) ambi

                  return $ layoutToGrid (plotHistogram 10 (map fromIntegral haskellValues))


plotShuffleAmbiguity :: Int -> Int -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotShuffleAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do source <- newStdGen
                  let values = generateShuffle (mkAmbiGen source (1 / fromIntegral samples) (0.0001)) samples
                  return $ combinedPlot range values


plotAmbiguity :: Int -> Int -> Integer -> IO (Renderable (LayoutPick Double Double Double))
plotAmbiguity runs samples range
  = fmap (gridToRenderable . aboveN) $ replicateM runs makePlot
  where
    makePlot :: IO (Grid (Renderable (LayoutPick Double Double Double)))
    makePlot = do source <- newStdGen
                  let values = generate (mkAmbiGen source (1 / fromIntegral samples) (0.0001)) samples
                  return $ combinedPlot range values


combinedPlot :: Integer -> [AmbiGenReal] -> Grid (Renderable (LayoutPick Double Double Double))
combinedPlot range values
  = histRealizations `beside` line `beside` histBinary `beside` hist
    where
      hist = layoutToGrid $ plotHistogram range samples
        where samples = map (fromIntegral . toRange (1, range)) values

      histBinary = layoutToGrid $ plotHistogram range samples
        where samples = map (fromIntegral . toRange (0, 1)) values

      histRealizations = layoutToGrid $ plotHistogram range values

      line = layoutToGrid $ plotRealizations (map realToFrac values)


plotHistogram :: Integer -> [Double] -> Layout Double Double
plotHistogram range samples
  = layout_plots .~ [hist] $ def
  where
    hist = histToPlot $
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
