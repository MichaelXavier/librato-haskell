{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
module Network.Librato.TypesSpec (spec) where

import ClassyPrelude
import Control.Lens ( (.~)
                    , (&))
import qualified Data.Attoparsec.Number as N
import Data.Aeson ( object
                  , Value(String)
                  , (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Network.Librato.Types
import Data.String.QQ (s)

import SpecHelper

import Test.Hspec

spec :: Spec
spec = do
  describe "QueryLike PaginationOptions" $ do
    it "renders the correct params" $
      toQuery defaultPagination `shouldBe` [ ("offset", Just "0")
                                           , ("length", Just "100")]

  describe "FromJSON Metric" $ do
    it "parses a gauge" $
      defaultGaugeString `shouldParseJSON` defaultGauge

    it "parses a counter" $
      defaultCounterString `shouldParseJSON` defaultCounter

  describe "FromJSON (PaginatedResponse Metric)" $ do
    it "parses the example documentation" $
      paginatedMetricsResponseString `shouldParseJSON` paginatedMetricsResponse

  describe "QueryLike (PaginatedRequest MetricsSearch)" $ do
    it "renders properly" $
      toQuery paginatedMetricsSearch `shouldBe` [ ("tags[]", Just "bar")
                                                , ("name", Just "foo")
                                                , ("offset", Just "0")
                                                , ("length", Just "100")]

  describe "FromJSON ErrorDetail" $ do
    it "parses ParamsErrors" $
      paramsErrorString `shouldParseJSON` paramsError

    it "parses RequestError" $
      requestErrorString `shouldParseJSON` requestError

    it "parses SystemErrors" $
      systemErrorString `shouldParseJSON` systemError

  describe "ToJSON Metrics" $
    it "renders the metrics in order under the gauges/counter keys" $
      Metrics [ defaultGauge
              , defaultCounter
              , defaultCounter & metricName .~ MetricName "moar_app_requests" ] `shouldGenerateJSON`
        object [
                 "gauges" .=  [
                   object [
                     "name"         .= String "cpu_temp"
                   , "period"       .= N.I 60
                   , "description"  .= String "Current CPU temperature in Fahrenheit"
                   , "display_name" .= String "cpu_temp"
                   , "source"       .= String "app1"
                   ]
                 ],
                 "counters" .= [
                   object [
                     "name"         .= String "app_requests"
                   , "period"       .= N.I 60
                   , "description"  .= String "HTTP requests serviced by the app per-minute"
                   , "display_name" .= String "app_requests"
                   , "source"       .= String "app1"
                   ],
                   object [
                     "name"         .= String "moar_app_requests"
                   , "period"       .= N.I 60
                   , "description"  .= String "HTTP requests serviced by the app per-minute"
                   , "display_name" .= String "app_requests"
                   , "source"       .= String "app1"
                   ]
                 ]
               ]

  describe "QueryLike MetricLookup" $ do
    it "renders the correct params" $
      toQuery fullMetricLookup `shouldBe` [ ("sources[]",         Just "source1")
                                          , ("sources[]",         Just "source2")
                                          , ("source_tag",        Just "st")
                                          , ("summarize_time",    Just "false")
                                          , ("summarize_sources", Just "true")
                                          ]
  -- measurement json doesn't inherently have source. and sometimes it changes (e.g. "ALL")
  describe "FromJSON Measurement" $ do
    it "parses the JSON correctly" $
      measurementString `shouldParseJSON` parsedMeasurement

  describe "FromJSON MetricSummarization" $ do
    it "parses the JSON correctly" $
      metricSummarizationString `shouldParseJSON` parsedMetricSummarization

fullMetricLookup :: MetricLookup
fullMetricLookup = MetricLookup "unimportant"
                                ["source1", "source2"]
                                (Just "st")
                                False
                                True
      

defaultPagination :: PaginationOptions
defaultPagination = def

paramsErrorString :: LBS.ByteString
paramsErrorString = [s|
{
  "errors": {
    "params": {
      "name":["is not present"],
      "start_time":["is not a number"]
    }
  }
}
|]

measurementString :: LBS.ByteString
measurementString = [s|
  {
    "measure_time": 1234567890,
    "value": 84.5,
    "count": 1
  }
|]

parsedMeasurement :: Measurement
parsedMeasurement = Measurement {
  _measurementTime  = 1234567890
, _measurementValue = 84.5
, _measurementCount = 1
}

metricSummarizationString :: LBS.ByteString
metricSummarizationString = [s|
{
  "resolution": 60,
  "measurements": {
    "server1.acme.com": [
      {
        "measure_time": 1234567890,
        "value": 84.5,
        "count": 1
      },
      {
        "measure_time": 1234567950,
        "value": 86.7,
        "count": 1
      }
    ],
    "server2.acme.com": [
      {
        "measure_time": 1234568010,
        "value": 84.6,
        "count": 1
      },
      {
        "measure_time": 1234568070,
        "value": 89.7,
        "count": 1
      }
    ]
  },
  "name": "cpu_temp",
  "display_name": "cpu_temp",
  "description": "Current CPU temperature in Fahrenheit",
  "period": 60,
  "type": "gauge",
  "attributes": {
    "created_by_ua": "librato-metrics/0.7.4 (ruby; 1.9.3p194; x86_64-linux) direct-faraday/0.8.4",
    "display_max": null,
    "display_min": 0,
    "display_stacked": true,
    "display_transform": null,
    "display_units_long": "Fahrenheit",
    "display_units_short": "&#176;F"
  }
}
|]

parsedMetricSummarization :: MetricSummarization
parsedMetricSummarization = MetricSummarization {
  _summarizationMetric = Gauge {
    _metricName        = MetricName "cpu_temp"
  , _metricDisplayName = "cpu_temp"
  , _metricDescription = "Current CPU temperature in Fahrenheit"
  , _metricPeriod      = 60
  , _metricSource      = Nothing
  }
, _summarizationMeasurements = [
  ("server1.acme.com", [ Measurement 1234567890 84.5 1
                       , Measurement 1234567950 86.7 1 ])
, ("server2.acme.com", [ Measurement 1234568010 84.6 1
                       , Measurement 1234568070 89.7 1 ])
]
}


paramsError :: ErrorDetail
paramsError = ParamsError [("name", ["is not present"]), ("start_time", ["is not a number"])]

requestErrorString :: LBS.ByteString
requestErrorString = [s|
{
  "errors": {
    "request": [
      "Please use secured connection through https!",
      "Please provide credentials for authentication."
    ]
  }
}
|]

requestError :: ErrorDetail
requestError = RequestError [ "Please use secured connection through https!"
                            , "Please provide credentials for authentication."]

systemErrorString :: LBS.ByteString
systemErrorString = [s|
{
  "errors": {
    "system": [
      "The API is currently down for maintenance. It'll be back shortly."
    ]
  }
}
|]

systemError :: ErrorDetail
systemError = SystemError ["The API is currently down for maintenance. It'll be back shortly."]

paginatedMetricsResponseString :: LBS.ByteString
paginatedMetricsResponseString = [s|
{
  "query":{
    "found":50,
    "length":10,
    "offset":20,
    "total":200,
    "params":{
      "name":"api"
    }
  },
  "metrics":[
    {
      "type": "counter",
      "period": 60,
      "attributes": {
        "display_min": 0,
        "display_transform": null,
        "display_units_short": "reqs",
        "created_by_ua": "librato-metrics/0.7.4 (ruby; 1.9.3p194; x86_64-linux) direct-faraday/0.8.4",
        "display_max": null,
        "display_units_long": "Requests",
        "display_stacked": true
      },
      "name": "app_requests",
      "description": "HTTP requests serviced by the app per-minute",
      "display_name": "app_requests",
      "source": "app1"
    },
    {
      "type": "gauge",
      "period": 60,
      "attributes": {
        "display_min": 0,
        "display_transform": null,
        "display_units_short": "&#176;F",
        "created_by_ua": "librato-metrics/0.7.4 (ruby; 1.9.3p194; x86_64-linux) direct-faraday/0.8.4",
        "display_max": null,
        "display_units_long": "Fahrenheit",
        "display_stacked": true
      },
      "name": "cpu_temp",
      "description": "Current CPU temperature in Fahrenheit",
      "display_name": "cpu_temp",
      "source": "app1"
    }
  ]
}
|]

paginatedMetricsResponse :: PaginatedResponse Metric
paginatedMetricsResponse = PaginatedResponse meta [ defaultCounter
                                                  , defaultGauge]
  where meta    = PaginationMeta 10 20 50

defaultCounter :: Metric
defaultCounter = Counter name 60 "HTTP requests serviced by the app per-minute" "app_requests" (Just "app1")
  where name = MetricName "app_requests"


defaultCounterString :: LBS.ByteString
defaultCounterString = [s|
  {
    "type": "counter",
    "period": 60,
    "attributes": {
      "display_min": 0,
      "display_transform": null,
      "display_units_short": "reqs",
      "created_by_ua": "librato-metrics/0.7.4 (ruby; 1.9.3p194; x86_64-linux) direct-faraday/0.8.4",
      "display_max": null,
      "display_units_long": "Requests",
      "display_stacked": true
    },
    "name": "app_requests",
    "description": "HTTP requests serviced by the app per-minute",
    "display_name": "app_requests",
    "source": "app1"
  }
|]

defaultGauge :: Metric
defaultGauge = Gauge name 60 "Current CPU temperature in Fahrenheit" "cpu_temp" (Just "app1")
  where name = MetricName "cpu_temp"

defaultGaugeString :: LBS.ByteString
defaultGaugeString = [s|
    {
      "type": "gauge",
      "period": 60,
      "attributes": {
        "display_min": 0,
        "display_transform": null,
        "display_units_short": "&#176;F",
        "created_by_ua": "librato-metrics/0.7.4 (ruby; 1.9.3p194; x86_64-linux) direct-faraday/0.8.4",
        "display_max": null,
        "display_units_long": "Fahrenheit",
        "display_stacked": true
      },
      "name": "cpu_temp",
      "description": "Current CPU temperature in Fahrenheit",
      "display_name": "cpu_temp",
      "source": "app1"
    }
|]

paginatedMetricsSearch :: PaginatedRequest MetricsSearch
paginatedMetricsSearch = PaginatedRequest defaultPagination defaultMetricsSearch

defaultMetricsSearch :: MetricsSearch
defaultMetricsSearch = MetricsSearch (Just "foo") [TagName "bar"]
