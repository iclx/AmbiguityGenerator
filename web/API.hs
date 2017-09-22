{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module API where

import Servant
import Servant.HTML.Lucid
import Lucid
import Data.List
import Data.Aeson
import Web.Internal.FormUrlEncoded
import qualified Network.HTTP.Media as M
import qualified Data.ByteString.Lazy.Char8 as BS


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


instance Show a => MimeRender PlainText [a] where
  mimeRender _ val = BS.pack (intercalate ", " (map show val))


instance Show a => MimeRender OctetStream [a] where
  mimeRender _ val = BS.pack (intercalate ", " (map show val))


instance Show a => MimeRender AmbigCSV [a] where
  mimeRender _ val = BS.pack (intercalate ", " (map show val))


data AmbigCSV


instance Accept AmbigCSV where
  contentType _ = "text" M.// "csv" M./: ("charset", "utf-8")


type FiniteAPI = "finite"
                 :> ReqBody '[FormUrlEncoded, JSON] FiniteData
                 :> Post '[AmbigCSV, JSON] (Headers '[Header "Content-Disposition" String] [Integer])


finiteAPI :: Proxy FiniteAPI
finiteAPI = Proxy


type RealizationAPI = "realizations"
                      :> ReqBody '[FormUrlEncoded, JSON] RealizationData
                      :> Post '[AmbigCSV, JSON] (Headers '[Header "Content-Disposition" String] [Double])


realizationAPI :: Proxy RealizationAPI
realizationAPI = Proxy


type WebPage = Get '[HTML] (Html ())


type AmbiguityAPI = FiniteAPI :<|> RealizationAPI :<|> WebPage


api :: Proxy AmbiguityAPI
api = Proxy
