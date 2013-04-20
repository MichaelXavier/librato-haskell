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
              , defaultCounter & metricName .~ "moar_app_requests" ] `shouldGenerateJSON`
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
defaultCounter = Counter "app_requests" 60 "HTTP requests serviced by the app per-minute" "app_requests" (Just "app1")


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
defaultGauge = Gauge "cpu_temp" 60 "Current CPU temperature in Fahrenheit" "cpu_temp" (Just "app1")

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
defaultMetricsSearch = MetricsSearch (Just "foo") [Tag "bar"]
