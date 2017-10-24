{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Data.Text as T
import Web.Internal.FormUrlEncoded
import qualified Network.HTTP.Media as M
import qualified Data.ByteString.Lazy.Char8 as BS


checkShuffled :: [Bool] -> Bool
checkShuffled [] = True
checkShuffled vs = any id vs


-- | Samples, lower bound, upper bound, shuffle, offLow, offHigh, draws to skip.
data FiniteData = FiniteData Int Integer Integer Bool AmbiGenReal AmbiGenReal Int
  deriving (Show, Eq)


instance ToJSON FiniteData where
  toJSON (FiniteData samples lower upper shuffled offLow offHigh skip)
    = object [ "samples" .= samples
             , "lower" .= lower
             , "upper" .= upper
             , "shuffled" .= shuffled
             , "offLow" .= offLow
             , "offHigh" .= offHigh
             , "skip" .= skip
             ]


instance FromJSON FiniteData where
  parseJSON (Object v) = FiniteData
    <$> v .: "samples"
    <*> (fromMaybe 0 <$> (v .:? "lower"))
    <*> (fromMaybe 1 <$> (v .:? "upper"))
    <*> (fromMaybe True <$> (v .:? "shuffled"))
    <*> (fromMaybe 0 <$> (v .:? "offLow"))
    <*> (fromMaybe 0 <$> (v .:? "offHigh"))
    <*> (fromMaybe 0 <$> (v .:? "skip"))


instance FromForm FiniteData where
  fromForm f = FiniteData
    <$> parseUnique "samples" f
    <*> (fromMaybe 0 <$> (parseMaybe "lower" f))
    <*> (fromMaybe 1 <$> (parseMaybe "upper" f))
    <*> (checkShuffled <$> (parseAll "shuffled" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offLow" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offHigh" f))
    <*> (fromMaybe 0 <$> (parseMaybe "skip" f))


instance ToForm FiniteData where
  toForm (FiniteData samples lower upper shuffled offLow offHigh skip) =
    [ ("samples", toQueryParam samples)
    , ("lower", toQueryParam lower)
    , ("upper", toQueryParam upper)
    , ("shuffled", toQueryParam shuffled)
    , ("offLow", toQueryParam offLow)
    , ("offHigh", toQueryParam offHigh)
    , ("skip", toQueryParam skip)
    ]


data RealizationData = RealizationData Int Bool AmbiGenReal AmbiGenReal Int
  deriving (Show, Eq)


instance ToJSON RealizationData where
  toJSON (RealizationData samples shuffled offLow offHigh skip)
    = object [ "samples" .= samples
             , "shuffled" .= shuffled
             , "offLow" .= offLow
             , "offHigh" .= offHigh
             , "skip" .= skip
             ]


instance FromJSON RealizationData where
  parseJSON (Object v) = RealizationData
    <$> v .: "samples"
    <*> (fromMaybe False <$> (v .:? "shuffled"))
    <*> (fromMaybe 0 <$> (v .:? "offLow"))
    <*> (fromMaybe 0 <$> (v .:? "offHigh"))
    <*> (fromMaybe 0 <$> (v .:? "skip"))


instance FromForm RealizationData where
  fromForm f = RealizationData
    <$> parseUnique "samples" f
    <*> (checkShuffled <$> (parseAll "shuffled" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offLow" f))
    <*> (fromMaybe 0 <$> (parseMaybe "offHigh" f))
    <*> (fromMaybe 0 <$> (parseMaybe "skip" f))


instance ToForm RealizationData where
  toForm (RealizationData samples shuffled offLow offHigh skip) =
    [ ("samples", toQueryParam samples)
    , ("shuffled", toQueryParam shuffled)
    , ("offLow", toQueryParam offLow)
    , ("offHigh", toQueryParam offHigh)
    , ("skip", toQueryParam skip)
    ]


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


type BitsAPI = "bits"
               :> ReqBody '[FormUrlEncoded, JSON] FiniteData
               :> Post '[AmbigCSV, JSON] (Headers '[Header "Content-Disposition" String] [Integer])


bitsAPI :: Proxy BitsAPI
bitsAPI = Proxy


type RealizationAPI = "realizations"
                      :> ReqBody '[FormUrlEncoded, JSON] RealizationData
                      :> Post '[AmbigCSV, JSON] (Headers '[Header "Content-Disposition" String] [Double])


realizationAPI :: Proxy RealizationAPI
realizationAPI = Proxy


type AmbiguityAPI = FiniteAPI :<|> RealizationAPI :<|> BitsAPI


ambiguityApi :: Proxy AmbiguityAPI
ambiguityApi = Proxy


type WebPage = Get '[HTML] (Html ())
type Docs = "docs" :> Get '[PlainText] T.Text

type SiteAPI = AmbiguityAPI :<|> Docs :<|> WebPage


api :: Proxy SiteAPI
api = Proxy
