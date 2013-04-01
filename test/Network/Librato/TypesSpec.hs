{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
module Network.Librato.TypesSpec (spec) where

import ClassyPrelude
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
  describe "FromJSON (PaginatedResponse Metric)" $ do
    it "parses the example documentation" $
      paginatedEmptyMetricsResponseString `shouldParseJSON` paginatedEmptyMetricsResponse

defaultPagination :: PaginationOptions
defaultPagination = def

paginatedEmptyMetricsResponseString :: LBS.ByteString
paginatedEmptyMetricsResponseString = [s|
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
      "display_name": "app_requests"
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
      "display_name": "cpu_temp"
    }
  ]
}
|]

paginatedEmptyMetricsResponse :: PaginatedResponse Metric
paginatedEmptyMetricsResponse = PaginatedResponse meta [counter, gauge]
  where meta    = PaginationMeta 10 20 50
        counter = Counter "app_requests" 60 "HTTP requests serviced by the app per-minute" "app_requests"
        gauge   = Gauge "cpu_temp" 60 "Current CPU temperature in Fahrenheit" "cpu_temp"
