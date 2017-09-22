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


downloadCsvButton :: Html ()
downloadCsvButton
  = div_ [class_ "card-action"] $
    input_ [type_ "submit", value_ "Download CSV", class_ "waves-effect waves-light btn"]


makeCard :: Html () -> Html ()
makeCard = div_ [class_ "col s6 card-panel hoverable"]


finiteForm :: Link -> Html ()
finiteForm link
  = let url = T.pack . show . linkURI $ link in
    makeCard $ do
    span_ [class_ "card-title"] "Finite Support"
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      input_ [type_ "number", name_ "lower", placeholder_ "lower", required_ ""]
      input_ [type_ "number", name_ "upper", placeholder_ "upper", required_ ""]
      downloadCsvButton


realizationForm :: Link -> Html ()
realizationForm link
  = let url = T.pack . show . linkURI $ link in
    makeCard $ do
    span_ [class_ "card-title"] "Raw Realizations"
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      downloadCsvButton


forms :: Link -> Link -> Html ()
forms finiteLink realizationLink
  = finite `mappend` realization
  where finite = finiteForm finiteLink
        realization = realizationForm realizationLink


blurb :: Html ()
blurb
  = div_ [class_ "card-panel hoverable"] $
    p_ "This website generates ambiguous random values in a CSV \
       \format. Additionally there is an API available."


homePage :: Link -> Link -> Html ()
homePage finiteLink realizationLink
  = div_ [class_ "container"] $
    blurb `mappend` forms finiteLink realizationLink
