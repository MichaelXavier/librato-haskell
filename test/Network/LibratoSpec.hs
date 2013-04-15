{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.LibratoSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.Librato

import Network.HTTPMock
import Network.HTTPMock.Expectations
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
  describe "GetAllMetrics" $ do
    it "request the correct path" $
      ("GET", "/v1/metrics") `shouldBeRequestedOnceBy`
          (runWithMocker_ noMetricsMocker $ getAllMetrics_')
    it "requests with HTTP basic authentication" $
      ("Authorization", encodedAuth) `shouldMakeRequestWithHeader`
          (runWithMocker_ noMetricsMocker $ getAllMetrics_')


    --describe "unauthorized request" $ do

--FIXME
noMetricsMocker :: HTTPMocker
noMetricsMocker = def & responder . fakedInteractions <>~ [emptyResponse]
  where emptyResponse = (matcher, AlwaysReturns response)
        matcher       = matchPathAndMethod "/v1/metrics" "GET"
        response      = FakeResponse status200 "{}" []

getAllMetrics_' = void $ runLibratoM testingConfig $ getAllMetrics def

testingConfig = ClientConfiguration "127.0.0.1" 4568 "/v1" "librato test" False username token

username    = "testuser"
token       = "testtoken"
encodedAuth = "Basic dGVzdHVzZXI6dGVzdHRva2Vu"

defaultMetricsSearch :: MetricsSearch
defaultMetricsSearch = def

fullMetricsSearch :: MetricsSearch
fullMetricsSearch = def & metricsNamed .~ (Just "foo")
                        & metricsSearchTags .~ [tag1, tag2]

tag1 :: Tag
tag1 = Tag "tagone"


tag2 :: Tag
tag2 = Tag "tagtwo"
