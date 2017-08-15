module Main where

import Ambiguity
import Graphics.Gnuplot.Simple
import System.Random


main :: IO ()
main
 = do source <- newStdGen
      let values = generate (mkAmbiGen source 0.0001 0.0001) 1000

      plotList [] values
