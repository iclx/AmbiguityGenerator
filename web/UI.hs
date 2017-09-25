{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lucid
import Lucid.Base
import Servant
import qualified Data.Text as T


materialize :: Html ()
materialize
  = link_ [rel_ "stylesheet", href_"https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"]
    `mappend`
    script_ [src_ "https://code.jquery.com/jquery-2.1.1.min.js"] ("" :: T.Text)
    `mappend`
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js"] ("" :: T.Text)


downloadCsvButton :: Html ()
downloadCsvButton
  = div_ [class_ "card-action"] $
    button_ [type_ "submit", class_ "btn waves-effect waves-light"] "Download CSV"


data_collapsible_ :: T.Text -> Attribute
data_collapsible_ = makeAttribute "data-collapsible"


shuffle :: Html ()
shuffle
  = div_ $ do
      "Shuffling: "
      div_ [class_ "switch"] $
        label_ $ do
          input_ [type_ "checkbox", name_ "shuffled", value_ "true", checked_]
          "Off"
          span_ [class_ "lever"] ""
          "On"


advancedSettings :: Html ()
advancedSettings
  = ul_ [class_ "collapsible z-depth-0", data_collapsible_ "expandable"] $ li_ $ do
      (div_ [class_ "collapsible-header"] $ do {i_ [class_ "material-icons"] "settings"; "Advanced settings"})
      (div_ [class_ "collapsible-body"] $ do
          shuffle
          div_ $ do
            div_ "Initial offset lower bound: "
            input_ [type_ "number", name_ "offLow", value_ "0", placeholder_ "initial offset lower bound", required_ ""]
          div_ $ do
            "Initial offset upper bound: "
            input_ [type_ "number", name_ "offHigh", value_ "0", placeholder_ "initial offset upper bound", required_ ""])


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
      advancedSettings
      downloadCsvButton


realizationForm :: Link -> Html ()
realizationForm link
  = let url = T.pack . show . linkURI $ link in
    makeCard $ do
    span_ [class_ "card-title"] "Raw Realizations"
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "samples", required_ ""]
      advancedSettings
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
  = html_ $ do
      head_ $ do
        materialize
        link_ [href_ "https://fonts.googleapis.com/icon?family=Material+Icons", rel_ "stylesheet"]
      body_ $ do
        div_ [class_ "container"] $ blurb `mappend` forms finiteLink realizationLink
