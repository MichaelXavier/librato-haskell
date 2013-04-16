{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.LibratoSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Debug.Trace (traceShow)
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
          (fst <$> getFromNoMetricsMocker)
    it "requests with HTTP basic authentication" $
      ("Authorization", encodedAuth) `shouldMakeRequestWithHeader`
          (fst <$> getFromNoMetricsMocker)

    describe "unauthorized request" $ do
      it "returns an UnauthorizedError" $
        (snd <$> getFromUnauthorizedMocker) `shouldReturn`
          Left UnauthorizedError

--FIXME: garbage

getFromNoMetricsMocker :: IO (HTTPMocker, LibratoResponse [Metric])
getFromNoMetricsMocker = runWithMocker_ noMetricsMocker $ getAllMetrics'

getFromUnauthorizedMocker :: IO (HTTPMocker, LibratoResponse [Metric])
getFromUnauthorizedMocker = runWithMocker_ unauthorizedMocker $ getAllMetrics'

noMetricsMocker :: HTTPMocker
noMetricsMocker = def & responder . fakedInteractions <>~ [emptyResponse]
  where emptyResponse = (matcher, AlwaysReturns response)
        matcher       = matchPathAndMethod "/v1/metrics" "GET"
        response      = FakeResponse status200 "{}" []

unauthorizedMocker :: HTTPMocker
unauthorizedMocker = def & responder . fakedInteractions <>~ [emptyResponse]
  where emptyResponse = (matcher, AlwaysReturns response)
        matcher       = matchPathAndMethod "/v1/metrics" "GET"
        response      = FakeResponse status401 "{}" []

getAllMetrics' = runLibratoM testingConfig $ getAllMetrics def

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
