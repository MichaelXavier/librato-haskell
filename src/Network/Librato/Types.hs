{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
module Network.Librato.Types ( LibratoM
                             , Tag(..)
                             , HasTag(..)
                             , Metric(..)
                             , HasMetric(..)
                             , ClientConfiguration(..)
                             , HasClientConfiguration(..)
                             , PaginationOptions(..)
                             , LibratoResponse(..)
                             , ErrorDetail(..)
                             , QueryLike(..)
                             , HasPaginationOptions(..)) where

import ClassyPrelude
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.State (StateT)
import Data.Aeson (FromJSON(..))
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

data PaginationOptions = PaginationOptions { _offset  :: Int
                                           , _perPage :: Int } deriving (Show, Eq)

makeClassy ''PaginationOptions

instance Default PaginationOptions where
  def = PaginationOptions 0 100

data Tag = Tag { _tagName :: Text } deriving (Show, Eq)

makeClassy ''Tag

data Metric = Counter { _metricName         :: Text
                      , _metricPeriod       :: Integer -- TODO: attributes
                      , _metricDescription  :: Text } |
              Gauge   { _metricName         :: Text
                      , _metricPeriod       :: Integer
                      , _metricDescription  :: Text } deriving (Show, Eq)

makeClassy ''Metric

type LibratoResponse a = Either ErrorDetail a

data ErrorDetail = ErrorDetail --TODO

instance FromJSON ErrorDetail where
  parseJSON = undefined


instance QueryLike PaginationOptions where
  toQuery po = [ ("offset", Just . encodeUtf8 . show $ po ^. offset)
               , ("length", Just . encodeUtf8 . show $ po ^. perPage)]
