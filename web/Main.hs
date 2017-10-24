{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module Main where

import Ambiguity
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs
import Servant.HTML.Lucid
import Lucid
import qualified Data.Text as T

import TextShow
import UI
import API
import Docs


ambiguityServer :: Server SiteAPI
ambiguityServer = (finite :<|> realizations :<|> bits) :<|> serveDocs :<|> servePage 
  where finite :: FiniteData -> Handler (Headers '[Header "Content-Disposition" String] [Integer])
        finite (FiniteData samples low high shuffled offLow offHigh skip)
          = do gen <- liftIO newStdGen

               let ambi = ambiSkip skip $ mkAmbiGen gen (1 / fromIntegral samples) 0.0001 offLow offHigh
               let values = if shuffled
                            then generateShuffleR ambi samples (low, high)
                            else generateR ambi samples (low, high)

               let filename = concat ["finite-", show samples, "-", show low, "-", show high, ".csv"]
               let header = concat ["filename=\"", filename, "\""]

               return $ addHeader header values


        bits :: FiniteData -> Handler (Headers '[Header "Content-Disposition" String] [Integer])
        bits (FiniteData samples low high shuffled offLow offHigh skip)
          = do gen <- liftIO newStdGen
               shuffleGen <- liftIO newStdGen

               let cont = mkContinuousState gen (1 / fromIntegral samples) 0.0001 offLow offHigh
               let values = if shuffled
                            then map fromIntegral . shuffleAndTake (samples + skip) shuffleGen $ generateBits cont
                            else map fromIntegral . take (samples + skip) $ generateBits cont

               let filename = concat ["bits-", show samples, "-", show low, "-", show high, ".csv"]
               let header = concat ["filename=\"", filename, "\""]

               return $ addHeader header (drop skip values)


        realizations :: RealizationData -> Handler (Headers '[Header "Content-Disposition" String] [Double])
        realizations (RealizationData samples shuffled offLow offHigh skip)
          = do gen <- liftIO newStdGen

               let ambi = ambiSkip skip $ mkAmbiGen gen (1 / fromIntegral samples) 0.0001 offLow offHigh
               let values = if shuffled
                            then generateShuffle ambi samples
                            else generate ambi samples

               let filename = concat ["realizations-", show samples, ".csv"]
               let header = concat ["filename=\"", filename, "\""]

               return $ addHeader header values

        servePage :: Handler (Html ())
        servePage = return $ homePage (safeLink api finiteAPI) (safeLink api realizationAPI)

        serveDocs :: Handler T.Text
        serveDocs = return . T.pack . markdown $ apiDocs


app :: Application
app = serve api ambiguityServer


main :: IO ()
main = do [port] <- fmap read <$> getArgs
          withStdoutLogger $ \aplogger -> do
            let settings = setPort port $ setLogger aplogger defaultSettings
            runSettings settings app
