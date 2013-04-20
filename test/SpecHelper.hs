{-# LANGUAGE NoImplicitPrelude    #-}
module SpecHelper ( shouldParseJSON
                  , shouldGenerateJSON) where

import ClassyPrelude

import Data.Aeson ( eitherDecode'
                  , ToJSON(..)
                  , Value
                  , FromJSON)
import qualified Data.ByteString.Lazy as LBS

import Test.Hspec

shouldParseJSON :: (FromJSON a, Show a, Eq a) => LBS.ByteString -> a -> Expectation
shouldParseJSON str obj = eitherDecode' str `shouldBe` Right obj

shouldGenerateJSON :: (ToJSON a) => a -> Value -> Expectation
shouldGenerateJSON x value = toJSON x `shouldBe` value
