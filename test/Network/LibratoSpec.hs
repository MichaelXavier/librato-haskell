{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.LibratoSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Debug.Trace (traceShow)
import Network.Librato

import Control.Rematch (equalTo)
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
      matchResultingMocker noMetricsMocker getAllMetrics' $
        allRequestsMatch [("GET", "/v1/metrics")]
    it "requests with HTTP basic authentication" $
      matchResultingMocker noMetricsMocker getAllMetrics' $
        hasRequestWithHeader ("Authorization", encodedAuth)

    describe "unauthorized request" $ do
      it "returns an UnauthorizedError" $
        matchResultFromMocker unauthorizedMocker getAllMetrics' $
          equalTo $ Left UnauthorizedError

  describe "createMetrics" $ do
    it "requests the correct path" $
      matchResultingMocker createMetricsMocker createMetrics' $
        allRequestsMatch [("POST", "/v1/metrics")]

    it "returns ()" $
      matchResultFromMocker createMetricsMocker createMetrics' $
        equalTo $ Right ()

    --TODO: JSON matcher?
    it "posts correct data" $
      matchResultingMocker createMetricsMocker createMetrics' $
        hasRequestWithBody "{\"gauges\":[{\"display_name\":\"Example Gauge\",\"name\":\"gauge_name\",\"period\":20,\"description\":\"gauge description\",\"source\":\"app1\"}],\"counters\":[{\"display_name\":\"Example Gauge\",\"name\":\"counter_name\",\"period\":20,\"description\":\"counter description\",\"source\":\"app1\"}]}"

    describe "validation error returned" $ do
      it "returns the appropriate type" $
        pendingWith "decide type"

  describe "getMetric" $ do
    it "uses the name in the path" $
      matchResultingMocker foundMetricMocker getMetric' $
        allRequestsMatch [("GET", "/v1/metrics/somemetric")]

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

createMetricsMocker :: HTTPMocker
createMetricsMocker = def & responder . fakedInteractions <>~ [emptyResponse]
  where emptyResponse = (matcher, AlwaysReturns response)
        matcher       = matchPathAndMethod "/v1/metrics" "POST"
        response      = FakeResponse status200 "" []

foundMetricMocker :: HTTPMocker
foundMetricMocker = def & responder . fakedInteractions <>~ [emptyResponse]
  where emptyResponse  = (matcher, AlwaysReturns response)
        matcher        = matchPathAndMethod "/v1/metrics/somemetric" "GET"
        response       = FakeResponse status200 renderedMetric []
        renderedMetric = "{}" --TODO

getAllMetrics' = runLibratoM testingConfig $ getAllMetrics def

getMetric' = runLibratoM testingConfig $ getMetric metricLookup 
  where metricLookup = MetricLookup "somemetric"
                                    ["source1", "source2"]
                                    (Just "sometag")
                                    False
                                    False

createMetrics' = runLibratoM testingConfig $ createMetrics [gauge, counter]
  where gauge   = Gauge "gauge_name" 20 "gauge description" "Example Gauge" (Just "app1")
        counter = Counter "counter_name" 20 "counter description" "Example Gauge" (Just "app1")

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
