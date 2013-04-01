{-# LANGUAGE NoImplicitPrelude    #-}
module SpecHelper (shouldParseJSON) where

import ClassyPrelude

import Data.Aeson ( eitherDecode'
                  , FromJSON)
import qualified Data.ByteString.Lazy as LBS

import Test.Hspec

shouldParseJSON :: (FromJSON a, Show a, Eq a) => LBS.ByteString -> a -> Expectation
shouldParseJSON str obj = eitherDecode' str `shouldBe` Right obj
