{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.Librato.TypesSpec (spec) where

import ClassyPrelude
import Data.Default
import Network.Librato.Types

import Test.Hspec

spec :: Spec
spec = do
  describe "QueryLike PaginationOptions" $ do
    it "renders the correct params" $
      toQuery defaultPagination `shouldBe` [ ("offset", Just "0")
                                           , ("length", Just "100")]

defaultPagination :: PaginationOptions
defaultPagination = def

