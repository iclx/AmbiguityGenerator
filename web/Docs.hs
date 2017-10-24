{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

module Docs where

import API
import Servant.Docs
import Data.Aeson
import Data.Text
import Lucid
import Data.Functor.Identity


apiDocs :: API
apiDocs = docsWithIntros [intro] ambiguityApi


intro :: DocIntro
intro =
  DocIntro "Ambiguity Generator" [blurb]
  where
    blurb = "This document goes over how to get ambiguous values from this \
            \API. It might be worth looking into the paper, which we would \
            \appreciate if you cited, https://doi.org/10.1287/mnsc.1100.1307"


instance ToSample FiniteData where
  toSamples _ = [("To get 1000 values with finite support from 0 to 10, with shuffling turned on, starting with a fixed offset of 0 for the sequence.", FiniteData 1000 0 10 True 0 0 0)]

instance ToSample RealizationData where
  toSamples _ = [("To get 1000 values of raw realizations, with shuffling turned on, starting with a fixed offset of 0 for the sequence.", RealizationData 1000 True 0 0 0)]


instance ToSample (HtmlT Identity ()) where
  toSamples _ = noSamples


instance ToSample Integer where
  toSamples _ = samples [1..10]


instance ToSample Char where
  toSamples _ = samples ['a'..'z']


instance ToSample Double where
  toSamples _ = samples [1..10]
