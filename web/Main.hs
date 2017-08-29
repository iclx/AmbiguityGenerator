{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Ambiguity
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy hiding (beside)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Grid
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


type FiniteAPI = "finite"
                 :> Capture "samples" Int
                 :> Capture "low" Integer
                 :> Capture "high" Integer
                 :> Post '[JSON] [Integer]


type RealizationAPI = "realizations"
                      :> Capture "samples" Int
                      :> Post '[JSON] [Double]


type AmbiguityAPI = FiniteAPI :<|> RealizationAPI


api :: Proxy AmbiguityAPI
api = Proxy


ambiguityServer :: Server AmbiguityAPI
ambiguityServer = finite :<|> realizations
  where finite :: Int -> Integer -> Integer -> Handler [Integer]
        finite samples low high
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generateR ambi samples (low, high)

               return values

        realizations :: Int -> Handler [Double]
        realizations samples
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generate ambi samples

               return values


app :: Application
app = serve api ambiguityServer


main :: IO ()
main = run 8001 app
