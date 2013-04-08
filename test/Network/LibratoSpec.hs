{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.LibratoSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.Librato

--import Network.HTTPMock
--import Network.HTTPMock.Expectations
import Test.Hspec

spec :: Spec
spec = do
  describe "Query parameter generation" $ do
    describe "QueryLike MetricsSearch" $ do
      it "renders the minimal case" $ do
        toQuery defaultMetricsSearch `shouldBe` []
      it "renders the complex case" $ do
        toQuery fullMetricsSearch `shouldBe` [ ("tags[]", Just "tagone")
                                             , ("tags[]", Just "tagtwo")
                                             , ("name", Just "foo")]
  --describe "GetAllMetrics" $ do
  --  describe "invalid credentials" $ do
  --    ("GET", "/v1/metrics") `shouldBeRequestedOnceBy`
  --      (runWithMocker_ noMetricsMocker $ getAllMetrics')

noMetricsMocker = undefined
getAllMetrics' = undefined
      

defaultMetricsSearch :: MetricsSearch
defaultMetricsSearch = def

fullMetricsSearch :: MetricsSearch
fullMetricsSearch = def & metricsNamed .~ (Just "foo")
                        & metricsSearchTags .~ [tag1, tag2]

tag1 :: Tag
tag1 = Tag "tagone"


tag2 :: Tag
tag2 = Tag "tagtwo"
