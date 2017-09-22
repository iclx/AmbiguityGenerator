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
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid

import TextShow
import UI
import API


ambiguityServer :: Server AmbiguityAPI
ambiguityServer = finite :<|> realizations :<|> servePage
  where finite :: FiniteData -> Handler (Headers '[Header "Content-Disposition" String] [Integer])
        finite (FiniteData samples low high)
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generateR ambi samples (low, high)

               let filename = concat ["finite-", show samples, "-", show low, "-", show high, ".csv"]
               let header = concat ["filename=\"", filename, "\""]

               return $ addHeader header values

        realizations :: RealizationData -> Handler (Headers '[Header "Content-Disposition" String] [Double])
        realizations (RealizationData samples)
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generate ambi samples

               let filename = concat ["realizations-", show samples, ".csv"]
               let header = concat ["filename=\"", filename, "\""]

               return $ addHeader header values

        servePage :: Handler (Html ())
        servePage = return $ materialize `mappend`
          ((finiteForm (safeLink api finiteAPI)) `mappend` (realizationForm (safeLink api realizationAPI)))


app :: Application
app = serve api ambiguityServer


main :: IO ()
main = do [port] <- fmap read <$> getArgs
          run port app

          
