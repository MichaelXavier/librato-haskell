{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Network.Librato.Types ( LibratoM
                             , Tag(..)
                             , HasTag(..)
                             , Metric(..)
                             , HasMetric(..)
                             , ClientConfiguration(..)
                             , HasClientConfiguration(..)
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
                             , HasPaginationOptions(..)) where

import ClassyPrelude
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.State (StateT)
import Data.Aeson ( FromJSON(..)
                  , withObject
                  , Object
                  , Value(..)
                  , (.:))
import Data.Aeson.Types (Parser)
import Data.Default
import Network.Http.Client ( Hostname
                           , Port)
import Network.HTTP.Types (QueryLike(..))

version :: ByteString
version = "0.0.0"

type LibratoM m a = StateT ClientConfiguration m a

data ClientConfiguration = ClientConfiguration { _apiHostname  :: Hostname
                                               , _apiPort      :: Port
                                               , _apiBasePath  :: ByteString
                                               , _apiUserAgent :: ByteString 
                                               , _apiUser      :: ByteString
                                               , _apiToken     :: ByteString } deriving (Show, Eq)

makeClassy ''ClientConfiguration

defaultConfiguration :: ByteString -> ByteString -> ClientConfiguration
defaultConfiguration = ClientConfiguration "metrics-api.librato.com" 80 "/v1" ua
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
                      , _metricPeriod       :: Integer -- TODO: attributes
                      , _metricDescription  :: Text
                      , _metricDisplayName  :: Text } |
              Gauge   { _metricName         :: Text
                      , _metricPeriod       :: Integer
                      , _metricDescription  :: Text
                      , _metricDisplayName  :: Text } deriving (Show, Eq)

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
          parseGauge obj = Gauge <$> obj .: "name"
                                 <*> obj .: "period"
                                 <*> obj .: "description"
                                 <*> obj .: "display_name"

type LibratoResponse a = Either ErrorDetail a

data ErrorDetail = ErrorDetail --TODO

instance FromJSON ErrorDetail where
  parseJSON = undefined

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

data MetricsSearch = MetricsSearch { _metricsNamed            :: Maybe Text -- case insensitive
                                   , _metricsSearchTags       :: [Tag] } deriving (Show, Eq)

makeClassy ''MetricsSearch

instance Default MetricsSearch where
  def = MetricsSearch Nothing empty

instance QueryLike MetricsSearch where
  toQuery ms = tagQueries ++ maybeToList nameQuery
    where nameQuery       = ("name", ) . Just . encodeUtf8 <$> ms ^. metricsNamed
          tagQueries      = map toTagQuery $ ms ^. metricsSearchTags
          toTagQuery      = ("tags[]",) . Just . encodeUtf8 . _tagName