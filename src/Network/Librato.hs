{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.Librato () where

import ClassyPrelude
import Control.Lens
import Data.Default
import Data.Aeson (FromJSON)

import Network.Librato.Types

-- TODO: figure out pagination

---- Metrics

data MetricsSearch = MetricsSearch { _metricsNamed            :: Maybe Text -- case insensitive
                                   , _metricsSearchPagination :: PaginationOptions
                                   , _tags                    :: [Tag] } deriving (Show, Eq)

instance Default MetricsSearch where
  def = MetricsSearch Nothing def empty

makeClassy ''MetricsSearch

--getMetrics :: MetricsSearch -> LibratoM (LibratoResponse [Metric])
--getMetrics = undefined
--
---- TODO: flesh out
--data MetricLookup = MetricLookup deriving (Show, Eq)
--
--getMetric :: MetricLookup -> LibratoM (LibratoResponse (Maybe Metric))
--getMetric = undefined
--
--createMetric :: Metric -> LibratoM (LibratoResponse ())
--createMetric = undefined
--
--deleteMetrics = undefined
--
--deleteMetric :: MetricName -> LibratoM (LibratoResponse ())
--deleteMetric = undefined
--
--updateMetric :: Metric -> LibratoM (LibratoResponse ())
--updateMetric = undefined
--
--
--getRequest :: (QueryLike params, FromJSON resp) => params -> ByteString -> LibratoM (LibratoResponse resp)
