{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
module Network.Librato.Types ( LibratoM
                             , Tag(..)
                             , HasTag(..)
                             , Metric(..)
                             , HasMetric(..)
                             , Metrics(..)
                             , HasMetrics(..)
                             , MetricLookup(..)
                             , HasMetricLookup(..)
                             , ClientConfiguration(..)
                             , HasClientConfiguration(..)
                             , defaultConfiguration
                             , PaginationOptions(..)
                             , PaginatedResponse(..)
                             , HasPaginatedResponse(..)
                             , PaginationMeta(..)
                             , HasPaginationMeta(..)
                             , LibratoResponse(..)
                             , ErrorDetail(..)
                             , QueryLike(..)
                             , PaginatedRequest(..)
                             , HasPaginatedRequest(..)
                             , MetricsSearch(..)
                             , HasMetricsSearch(..)
                             , HasPaginationOptions(..)
                             , MetricSummarization(..)
                             , HasMetricSummarization(..)
                             , Measurement(..)
                             , HasMeasurement(..)) where

import ClassyPrelude
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson ( FromJSON(..)
                  , withObject
                  , Object
                  , Value(..)
                  , ToJSON(..)
                  , object
                  , (.:?)
                  , (.=)
                  , (.:))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as H
import Data.Default
import Data.Time.Clock.POSIX (POSIXTime(..))
import Network.Http.Client ( Hostname
                           , Port)
import Network.HTTP.Types (QueryLike(..))

version :: ByteString
version = "0.0.0"

type LibratoM m a = ReaderT ClientConfiguration m a

data ClientConfiguration = ClientConfiguration { _apiHostname  :: Hostname
                                               , _apiPort      :: Port
                                               , _apiBasePath  :: ByteString
                                               , _apiUserAgent :: ByteString 
                                               , _apiUseSSL    :: Bool 
                                               , _apiUser      :: ByteString
                                               , _apiToken     :: ByteString } deriving (Show, Eq)

makeClassy ''ClientConfiguration

defaultConfiguration :: ByteString -> ByteString -> ClientConfiguration
defaultConfiguration = ClientConfiguration "metrics-api.librato.com" 443 "/v1" ua True
  where ua = "Network.Librato/" ++ version ++ " (haskell)"

--TODO: ordering

data PaginationOptions = PaginationOptions { _offset  :: Integer
                                           , _perPage :: Integer } deriving (Show, Eq)

makeClassy ''PaginationOptions

instance Default PaginationOptions where
  def = PaginationOptions 0 100

data Tag = Tag { _tagName :: Text } deriving (Show, Eq)

makeClassy ''Tag

--TODO: attributes
data Metric = Counter { _metricName         :: Text
                      , _metricPeriod       :: Integer -- TODO: attributes?
                      , _metricDescription  :: Text
                      , _metricDisplayName  :: Text
                      , _metricSource       :: Maybe Text } | --am i conflating source with some other resource?
              Gauge   { _metricName         :: Text
                      , _metricPeriod       :: Integer
                      , _metricDescription  :: Text
                      , _metricDisplayName  :: Text
                      , _metricSource       :: Maybe Text } deriving (Show, Eq)

makeClassy ''Metric

instance FromJSON Metric where
  parseJSON = withObject "Metric" parseMetric
    where parseMetric obj = case lookup "type" obj of
                              Just (String "counter") -> parseCounter obj
                              Just (String "gauge")   -> parseGauge obj
                              _                       -> fail "Invalid or missing type key"
          parseCounter obj = Counter <$> obj .: "name"
                                     <*> obj .: "period"
                                     <*> obj .: "description"
                                     <*> obj .: "display_name"
                                     <*> obj .:? "source"
          parseGauge obj = Gauge <$> obj .: "name"
                                 <*> obj .: "period"
                                 <*> obj .: "description"
                                 <*> obj .: "display_name"
                                 <*> obj .:? "source"

instance ToJSON Metric where
  toJSON m = object [ "name"         .= (m ^. metricName)
                    , "period"       .= (m ^. metricPeriod)
                    , "description"  .= (m ^. metricDescription)
                    , "display_name" .= (m ^. metricDisplayName)
                    , "source"       .= (m ^. metricSource) ]

type LibratoResponse a = Either ErrorDetail a

data ErrorDetail = ParseError Text               |
                   ParamsError [(Text, [Text])]  |
                   RequestError [Text]           |
                   SystemError [Text]            |
                   OtherError                    | -- TODO
                   AlreadyExistsError            |
                   MaintenanceError              |
                   UnauthorizedError deriving (Show, Eq, Typeable) -- TODO: more

instance Exception ErrorDetail

parseAtKey :: FromJSON a => Text -> Object -> (Object -> Parser a) -> Parser a
parseAtKey key obj parser = maybe failure parser' lookupKey
  where lookupKey = H.lookup key obj
        failure   = fail $ "Could not find key " ++ keyStr
        parser'   = withObject keyStr parser
        keyStr    = unpack key
                            

instance FromJSON ErrorDetail where
  parseJSON = withObject "ErrorDetail" $ parseError
    where parseError o = unwrapError o $ \v ->
                           parseParamsError v  <|>
                           parseRequestError v <|>
                           parseSystemError v  <|>
                           pure OtherError --TODO
          unwrapError = parseAtKey "errors"
          parseParamsError o  = ParamsError  <$> H.toList <$> (o .: "params")
          parseRequestError o = RequestError <$> o .: "request"
          parseSystemError o  = SystemError  <$> o .: "system"

instance QueryLike PaginationOptions where
  toQuery po = [ ("offset", Just . encodeUtf8 . show $ po ^. offset)
               , ("length", Just . encodeUtf8 . show $ po ^. perPage)]

-- if offset + length < found, there is more
data PaginationMeta = PaginationMeta {
    _responseLength :: Integer
  , _responseOffset  :: Integer
  , _responseFound   :: Integer
  -- purposefully omitting total as it is useless to pagination
} deriving (Show, Eq)

makeClassy ''PaginationMeta

instance FromJSON PaginationMeta where
  parseJSON = withObject "PaginationMeta" parseMeta
    where parseMeta o = PaginationMeta <$> o .: "length"
                                       <*> o .: "offset"
                                       <*> o .: "found"

data PaginatedResponse a = PaginatedResponse {
    _responseMeta      :: PaginationMeta
  , _paginationPayload :: [a]
} deriving (Show, Eq)

makeClassy ''PaginatedResponse

instance FromJSON (PaginatedResponse Metric) where
  parseJSON = parsePaginatedResponse "Metric" "metrics"

parsePaginatedResponse typeName payloadKey = withObject typeName parseResponse
  where parseResponse obj = PaginatedResponse <$> obj .: "query"
                                              <*> obj .: payloadKey

data PaginatedRequest a = PaginatedRequest {
  _requestPagination :: PaginationOptions
, _requestQuery      :: a
} deriving (Show, Eq)

makeClassy ''PaginatedRequest

instance QueryLike a => QueryLike (PaginatedRequest a) where
  toQuery po = toQuery innerQuery ++ toQuery pagination
    where pagination = po ^. requestPagination
          innerQuery = po ^. requestQuery

instance Default a => Default (PaginatedRequest a) where
  def = PaginatedRequest def def

--TODO: time interval stuff
data MetricLookup = MetricLookup {
  _metricLookupName             :: Text
, _metricLookupSources          :: [Text]
, _metricLookupSourceTag        :: Maybe Text
, _metricLookupSummarizeTime    :: Bool
, _metricLookupSummarizeSources :: Bool
} deriving (Show, Eq)

makeClassy ''MetricLookup

-- is it a bad idea when there's no sensible name?
instance Default MetricLookup where
  def = MetricLookup empty empty def False False

instance QueryLike MetricLookup where
  toQuery ml = sourcesQuery ++ [ ("source_tag",        encodeUtf8 <$> st)
                               , ("summarize_time",    Just sumTime)
                               , ("summarize_sources", Just sumSrc)
                               ]
    where st           = ml ^. metricLookupSourceTag
          sumTime      = b2s $ ml ^. metricLookupSummarizeTime
          sumSrc       = b2s $ ml ^. metricLookupSummarizeSources
          b2s True     = "true"
          b2s False    = "false"
          sourcesQuery = map (("sources[]",) . Just . encodeUtf8) sources
          sources      = ml ^. metricLookupSources

data MetricsSearch = MetricsSearch {
  _metricsNamed            :: Maybe Text -- case insensitive
, _metricsSearchTags       :: [Tag]
} deriving (Show, Eq)

makeClassy ''MetricsSearch

instance Default MetricsSearch where
  def = MetricsSearch Nothing empty

instance QueryLike MetricsSearch where
  toQuery ms = tagQueries ++ maybeToList nameQuery
    where nameQuery       = ("name", ) . Just . encodeUtf8 <$> ms ^. metricsNamed
          tagQueries      = map toTagQuery $ ms ^. metricsSearchTags
          toTagQuery      = ("tags[]",) . Just . encodeUtf8 . _tagName


newtype Metrics = Metrics { _unMetrics :: [Metric] }

makeClassy ''Metrics

instance ToJSON Metrics where
  toJSON ms = object $ gaugesObj ++ countersObj
    where gaugesObj
            | not $ null gauges = ["gauges" .= gauges]
            | otherwise         = [] --TODO
          countersObj
            | not $ null counters = ["counters" .= counters]
            | otherwise           = [] --TODO
          (gauges, counters) = partition isGauge $ ms ^. unMetrics
          isGauge Gauge {} = True
          isGauge _        = False

data Measurement = Measurement {
  _measurementTime  :: POSIXTime --TODO: should I use UTCTime instead?
, _measurementValue :: Double -- is double the correct type?
, _measurementCount :: Int
} deriving (Show, Eq) --TODO

makeClassy ''Measurement

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" parseMeasurement
    where parseMeasurement o = Measurement <$> (toPOSIXTime <$> o .: "measure_time")
                                           <*> o .: "value"
                                           <*> o .: "count"
          toPOSIXTime :: Integer -> POSIXTime
          toPOSIXTime = fromIntegral

--TODO: attributes
--TODO: where does resolution come from?
--TODO: should measurements be a HashMap?
data MetricSummarization = MetricSummarization {
  _summarizationMetric       :: Metric
, _summarizationMeasurements :: [(Text, [Measurement])] -- source seems like maybe it should have its own type
} deriving (Show, Eq)

makeClassy ''MetricSummarization

instance FromJSON MetricSummarization where
  parseJSON = withObject "MetricSummarization" parseSummarization
    where parseSummarization o = MetricSummarization <$> parseJSON (Object o)
                                                     <*> (H.toList <$> o .: "measurements")

