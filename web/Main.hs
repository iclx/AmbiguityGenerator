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
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Media as M
import Servant
import Data.Aeson
import GHC.Generics
import Web.Internal.FormUrlEncoded
import Lucid
import Servant.HTML.Lucid
import qualified Data.Text as T
import TextShow


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


finiteForm :: Link -> Html ()
finiteForm link
  = let url = T.pack . show . linkURI $ link in
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      input_ [type_ "number", name_ "lower", placeholder_ "lower", required_ ""]
      input_ [type_ "number", name_ "upper", placeholder_ "upper", required_ ""]
      input_ [type_ "submit", value_ "Download CSV"]


realizationForm :: Link -> Html ()
realizationForm link
  = let url = T.pack . show . linkURI $ link in
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      input_ [type_ "submit", value_ "Download CSV"]


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
        servePage = return $
          ((finiteForm (safeLink api finiteAPI)) `mappend` (realizationForm (safeLink api realizationAPI)))


app :: Application
app = serve api ambiguityServer


main :: IO ()
main = do [port] <- fmap read <$> getArgs
          run port app
