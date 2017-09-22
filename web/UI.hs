{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lucid
import Servant
import qualified Data.Text as T


materialize :: Html ()
materialize
  = link_ [rel_ "stylesheet", href_"https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"]
    `mappend`
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js"] ("" :: T.Text)


finiteForm :: Link -> Html ()
finiteForm link
  = let url = T.pack . show . linkURI $ link in
    div_ [class_ "card-panel hoverable"] $
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      input_ [type_ "number", name_ "lower", placeholder_ "lower", required_ ""]
      input_ [type_ "number", name_ "upper", placeholder_ "upper", required_ ""]
      input_ [type_ "submit", value_ "Download CSV"]


realizationForm :: Link -> Html ()
realizationForm link
  = let url = T.pack . show . linkURI $ link in
    div_ [class_ "card-panel hoverable"] $
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      input_ [type_ "submit", value_ "Download CSV"]


blurb :: Html ()
blurb
  = div_ [class_ "card-panel hoverable"] $
    p_ "This website generates ambiguous random values in a CSV\
       \format. Additionally there is an API available."


homePage :: Link -> Link -> Html ()
homePage finiteLink realizationLink
  = blurb `mappend` finiteForm finiteLink `mappend` realizationForm realizationLink
