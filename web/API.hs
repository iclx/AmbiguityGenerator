{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module API where

import Ambiguity
import Servant
import Servant.HTML.Lucid
import Lucid
import Data.List
import Data.Aeson
import Data.Maybe
import Web.Internal.FormUrlEncoded
import qualified Network.HTTP.Media as M
import qualified Data.ByteString.Lazy.Char8 as BS


-- | Samples, lower bound, upper bound, shuffle, offLow, offHigh.
data FiniteData = FiniteData Int Integer Integer Bool AmbiGenReal AmbiGenReal
  deriving (Show, Eq)


instance ToJSON FiniteData where
  toJSON (FiniteData samples lower upper shuffled offLow offHigh)
    = object [ "samples" .= samples
             , "lower" .= lower
             , "upper" .= upper
             , "shuffled" .= shuffled
             , "offLow" .= offLow
             , "offHigh" .= offHigh
             ]


instance FromJSON FiniteData where
  parseJSON (Object v) = FiniteData
    <$> v .: "samples"
    <*> v .: "lower"
    <*> v .: "upper"
    <*> (fromMaybe False <$> (v .:? "shuffled"))
    <*> (fromMaybe 0 <$> (v .:? "offLow"))
    <*> (fromMaybe 0 <$> (v .:? "offHigh"))


instance FromForm FiniteData where
  fromForm f = FiniteData
    <$> parseUnique "samples" f
    <*> parseUnique "lower" f
    <*> parseUnique "upper" f
    <*> (fromMaybe False <$> (parseMaybe "shuffled" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offLow" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offHigh" f))


data RealizationData = RealizationData Int Bool AmbiGenReal AmbiGenReal
  deriving (Show, Eq)


instance ToJSON RealizationData where
  toJSON (RealizationData samples shuffled offLow offHigh)
    = object [ "samples" .= samples
             , "shuffled" .= shuffled
             , "offLow" .= offLow
             , "offHigh" .= offHigh
             ]


instance FromJSON RealizationData where
  parseJSON (Object v) = RealizationData
    <$> v .: "samples"
    <*> (fromMaybe False <$> (v .:? "shuffled"))
    <*> (fromMaybe 0 <$> (v .:? "offLow"))
    <*> (fromMaybe 0 <$> (v .:? "offHigh"))


instance FromForm RealizationData where
  fromForm f = RealizationData
    <$> parseUnique "samples" f
    <*> (fromMaybe False <$> (parseMaybe "shuffled" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offLow" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offHigh" f))


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
