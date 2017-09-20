{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ambiguity
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Aeson
import GHC.Generics
import Web.Internal.FormUrlEncoded
import Data.Text


-- | Samples, lower bound, upper bound.
data FiniteData = FiniteData Int Integer Integer
  deriving (Show, Eq)


instance ToJSON FiniteData where
  toJSON (FiniteData samples lower upper)
    = object [ "samples" .= samples
             , "lower" .= lower
             , "upper" .= upper
             ]


instance FromJSON FiniteData where
  parseJSON (Object v) = FiniteData
    <$> v .: "samples"
    <*> v .: "lower"
    <*> v .: "upper"


instance FromForm FiniteData where
  fromForm f = FiniteData
    <$> parseUnique "samples" f
    <*> parseUnique "lower" f
    <*> parseUnique "upper" f


data RealizationData = RealizationData Int
  deriving (Show, Eq)


instance ToJSON RealizationData where
  toJSON (RealizationData samples)
    = object [ "samples" .= samples ]


instance FromJSON RealizationData where
  parseJSON (Object v) = RealizationData
    <$> v .: "samples"


instance FromForm RealizationData where
  fromForm f = RealizationData
    <$> parseUnique "samples" f


type FiniteAPI = "finite"
                 :> ReqBody '[FormUrlEncoded, JSON] FiniteData
                 :> Post '[JSON] [Integer]


type RealizationAPI = "realizations"
                      :> ReqBody '[FormUrlEncoded, JSON] RealizationData
                      :> Post '[JSON] [Double]


type AmbiguityAPI = FiniteAPI :<|> RealizationAPI


api :: Proxy AmbiguityAPI
api = Proxy


ambiguityServer :: Server AmbiguityAPI
ambiguityServer = finite :<|> realizations
  where finite :: FiniteData -> Handler [Integer]
        finite (FiniteData samples low high)
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generateR ambi samples (low, high)

               return values

        realizations :: RealizationData -> Handler [Double]
        realizations (RealizationData samples)
          = do gen <- liftIO newStdGen

               let ambi = mkAmbiGen gen 0 0
               let values = generate ambi samples

               return values


app :: Application
app = serve api ambiguityServer


main :: IO ()
main = run 8001 app
