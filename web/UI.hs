{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lucid
import Lucid.Base
import Servant
import Data.Monoid
import qualified Data.Text as T


baseUrl :: Html ()
baseUrl = "ambiguity.typesofnote.com"


materialize :: Html ()
materialize
  = mconcat
    [ link_ [rel_ "stylesheet", href_"https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"]
    , script_ [src_ "https://code.jquery.com/jquery-2.1.1.min.js"] ("" :: T.Text)
    , script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js"] ("" :: T.Text)
    ]


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
          input_ [type_ "hidden", name_ "shuffled", value_ "false"]
          input_ [type_ "checkbox", name_ "shuffled", value_ "true", checked_]
          "Off"
          span_ [class_ "lever"] ""
          "On"


advancedSettings :: Html ()
advancedSettings
  = ul_ [class_ "collapsible z-depth-0", data_collapsible_ "expandable"] $ li_ $ do
      div_ [class_ "collapsible-header"] $ do {i_ [class_ "material-icons"] "settings"; "Advanced settings"}
      div_ [class_ "collapsible-body"] $ do
        shuffle
        div_ $ do
          div_ "Initial offset lower bound: "
          input_ [type_ "number", name_ "offLow", value_ "0", placeholder_ "initial offset lower bound", required_ ""]
        div_ $ do
          "Initial offset upper bound: "
          input_ [type_ "number", name_ "offHigh", value_ "0", placeholder_ "initial offset upper bound", required_ ""]


makeExample :: Html () -> Html () -> Html ()
makeExample title example
  = ul_ [class_ "collection with-header"] $ do
      li_ [class_ "collection-header"] $ h5_ [class_ "title"] $ title
      li_ [class_ "collection-item"] $ code_ [class_ "prettyprint"] $ example


examples :: Html ()
examples
  = mconcat
    [ makeExample "Curl" ("curl -H \"Content-Type: application/json\" \
                          \-X POST -d '{\"samples\":1000, \"lower\":0, \
                          \\"upper\":10}' " <> baseUrl <> "/finite")

    , makeExample "Python" ("import requests; r=requests.post('http://" <>
                            baseUrl <>
                            "/finite', data={\"samples\":1000, \"lower\":0, \
                            \\"upper\":10})")

    , makeExample "Mathematica" ("ToExpression[StringJoin[\"{\", \
                                 \URLExecute[\"http://" <> baseUrl <> "/finite\"\
                                 \, {\"format\" -> \"json\", \"samples\" -> 1000, \
                                 \\"lower\" -> 0, \"upper\" -> 10, \
                                 \\"shuffled\" -> false}, \"Method\" -> \"POST\"]\
                                 \, \"}\"]]")
    ]


exampleCollapsible :: Html ()
exampleCollapsible
  = ul_ [class_ "collapsible z-depth-0", data_collapsible_ "expandable"] $ li_ $ do
      div_ [class_ "collapsible-header"] $ do {i_ [class_ "material-icons"] "code"; "API examples"}
      div_ [class_ "collapsible-body"] $ examples


makeCard :: Html () -> Html ()
makeCard = div_ [class_ "card col s6 hoverable"]


finiteForm :: Link -> Html ()
finiteForm link
  = let url = T.pack . show . linkURI $ link in
    div_ $ do
    span_ [class_ "card-title"] "Finite Support"
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "number of draws", required_ ""]
      input_ [type_ "number", name_ "lower", placeholder_ "lower bound", required_ ""]
      input_ [type_ "number", name_ "upper", placeholder_ "upper bound", required_ ""]
      advancedSettings
      downloadCsvButton


realizationForm :: Link -> Html ()
realizationForm link
  = let url = T.pack . show . linkURI $ link in
    div_ $ do
    span_ [class_ "card-title"] "Raw Realizations"
    form_ [method_ "POST", action_ url] $ do
      input_ [type_ "number", name_ "samples", placeholder_ "number of draws", required_ ""]
      advancedSettings
      downloadCsvButton


forms :: Link -> Link -> Html ()
forms finiteLink realizationLink
  = makeCard $ do
      div_ [class_ "card-content"] $
        p_ "Download CSV files of ambiguous values. Either with finite support, \
           \or the raw real number realizations from the generator."
      div_ [class_ "card-tabs"] $
        ul_ [class_ "tabs tabs-fixed-width"] $ do
          li_ [class_ "tab"] $ a_ [href_"#realization-form"] "Raw Realizations"
          li_ [class_ "tab"] $ a_ [href_"#finite-form"] "Finite Support"
      div_ [class_ "card-content"] $ do
        finite
        realization
  where finite = div_ [id_ "finite-form"] $ finiteForm finiteLink
        realization = div_ [id_ "realization-form"] $ realizationForm realizationLink


blurb :: Html ()
blurb
  = makeCard $ do
      div_ [class_ "card-content"] $ do
        "This website generates ambiguous random values in a CSV \
        \format. Don't know what ambiguous random values are? You \
        \can read about them in the paper "
        a_ [href_ "https://doi.org/10.1287/mnsc.1100.1307"] "here"
        ", which we encourage you to cite!"
        p_ "Here's some other links which you might find useful:"
        ol_ $ do
          li_ (a_ [href_ "https://doi.org/10.1287/mnsc.1100.1307"] "Paper")
          li_ (a_ [href_ "https://github.com/HaskellAmbiguity/AmbiguityGenerator"] "Source Code")
          li_ (a_ [href_ "docs"] "Auto-Generated API Docs")
        exampleCollapsible


homePage :: Link -> Link -> Html ()
homePage finiteLink realizationLink
  = html_ $ do
      head_ $ do
        materialize
        link_ [href_ "https://fonts.googleapis.com/icon?family=Material+Icons", rel_ "stylesheet"]
        script_ [src_ "https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"] ("" :: T.Text)
      body_ $ do
        div_ [class_ "container"] $ blurb `mappend` forms finiteLink realizationLink
