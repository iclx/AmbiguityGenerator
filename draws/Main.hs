module Main where

import Ambiguity
import System.Random
import System.Environment
import Control.Monad
import Data.List


main :: IO ()
main
 = do [runsStr, samplesStr, filepath] <- getArgs

      let runs = read runsStr
      let samples = read samplesStr

      makeDraws filepath runs samples


drawBits :: String -> Int -> Int -> IO ()
drawBits fileBase samples run
  = do gen <- newStdGen
       let ambi = mkAmbiGen gen 0 0

       let values = generateR ambi samples (0,1)
       let output = intercalate ", " (map show values)

       writeFile (fileBase ++ show samples ++ "-bits-" ++ show run ++ ".csv") output


drawDigits :: String -> Int -> Int -> IO ()
drawDigits fileBase samples run
  = do gen <- newStdGen
       let ambi = mkAmbiGen gen 0 0

       let values = generateR ambi samples (0,9)
       let output = intercalate ", " (map show values)

       writeFile (fileBase ++ show samples ++ "-digits-" ++ show run ++ ".csv") output


drawAmbiguous :: String -> Int -> Int -> IO ()
drawAmbiguous fileBase samples run
  = do gen <- newStdGen
       let ambi = mkAmbiGen gen 0 0

       let values = generate ambi samples
       let output = intercalate ", " (map show values)

       writeFile (fileBase ++ show samples ++ "-ambiguous-" ++ show run ++ ".csv") output


makeDraws :: String -> Int-> Int -> IO ()
makeDraws fileBase samples runs
  = do mapM_ (drawBits fileBase samples) [1..runs]
       mapM_ (drawDigits fileBase samples) [1..runs]
       mapM_ (drawBits fileBase samples) [1..runs]
