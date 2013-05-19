{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Librato.Types ( LibratoM
                             , Tag(..)
                             , ID(..)
                             , Unit(..)
                             , HasID(..)
                             , HasTag(..)
                             , Metric(..)
                             , HasMetric(..)
                             , Metrics(..)
                             , HasMetrics(..)
                             , MetricName(..)
                             , unMetricName
                             , MetricNames(..)
                             , HasMetricNames(..)
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
                             , requestPagination
                             , requestQuery
                             , MetricsSearch(..)
                             , HasMetricsSearch(..)
                             , HasPaginationOptions(..)
                             , MetricSummarization(..)
                             , HasMetricSummarization(..)
                             , Measurement(..)
                             , Dashboard(..)
                             , HasDashboard(..)
                             , NewDashboard(..)
                             , LDashboard(..)
                             , Instrument(..)
                             , HasInstrument(..)
                             , NewInstrument(..)
                             , LInstrument(..)
                             , ServiceType(..)
                             , Service(..)
                             , HasService(..)
                             , NewService(..)
                             , LService(..)
                             , ChartEntityType(..)
                             , ChartToken(..)
                             , HasChartToken(..)
                             , NewChartToken(..)
                             , LChartToken(..)
                             , User(..)
                             , HasUser(..)
                             , NewUser(..)
                             , LUser(..)
                             , HasMeasurement(..)) where

import ClassyPrelude
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson ( FromJSON(..)
                  , withObject
                  , withText
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

newtype ID = ID { _unID :: Text } deriving (Show, Eq, FromJSON, ToJSON)

makeClassy ''ID

data Unit = Unit deriving (Show, Eq)

data Tag = Tag { _tagName :: Text } deriving (Show, Eq)

makeClassy ''Tag

newtype MetricName = MetricName { _unMetricName :: Text } deriving (Show, Eq, FromJSON, ToJSON)

makeLenses ''MetricName

--TODO: attributes
data Metric = Counter { _metricName         :: MetricName
                      , _metricPeriod       :: Integer -- TODO: attributes?
                      , _metricDescription  :: Text
                      , _metricDisplayName  :: Text
                      , _metricSource       :: Maybe Text } | --am i conflating source with some other resource?
              Gauge   { _metricName         :: MetricName
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

--TODO: fields
data Instrument a = Instrument { _instrumentID :: a } deriving (Show, Eq)

makeClassy ''Instrument

type NewInstrument = Instrument ()
type LInstrument   = Instrument ID

instance FromJSON LInstrument where
  parseJSON = undefined

instance ToJSON a => ToJSON (Instrument a) where
  toJSON = undefined

data Dashboard a = Dashboard { _dashboardID :: a
                             , _dashboardName :: Text
                             , _dashboardInstruments :: [Instrument a] } deriving (Show, Eq)

makeClassy ''Dashboard

type NewDashboard = Dashboard ()
type LDashboard   = Dashboard ID

instance FromJSON LDashboard where
  parseJSON = withObject "LDashboard" parseLDashboard
    where parseLDashboard obj = Dashboard <$> obj .: "id"
                                          <*> obj .: "name"
                                          <*> obj .: "instruments"

instance ToJSON a => ToJSON (Dashboard a) where
  toJSON d = object [ "id"          .= (d ^. dashboardID)
                    , "name"        .= (d ^. dashboardName)
                    , "instruments" .= (d ^. dashboardInstruments) ]

data ServiceType = CampfireService  |
                   HipChatService   |
                   MailService      |
                   OpsGenieService  |
                   PagerDutyService |
                   WebhookService deriving (Show, Eq)

instance FromJSON ServiceType where
  parseJSON = withText "ServiceType" parseServiceType
    where parseServiceType "campfire"  = pure CampfireService
          parseServiceType "hipchat"   = pure HipChatService
          parseServiceType "opsgenie"  = pure OpsGenieService
          parseServiceType "pagerduty" = pure PagerDutyService
          parseServiceType "webhook"   = pure WebhookService
          parseServiceType s           = fail $ "unexpected service type " <> unpack s

instance ToJSON ServiceType where
  toJSON CampfireService  = "campfire"
  toJSON HipChatService   = "hipchat"
  toJSON OpsGenieService  = "opsgenie"
  toJSON PagerDutyService = "pagerduty"
  toJSON WebhookService   = "webhook"

data Service a = Service { _serviceID       :: a
                         , _serviceType     :: ServiceType
                         , _serviceSettings :: Object
                         , _serviceTitle    :: Text } deriving (Show, Eq)

makeClassy ''Service

type NewService = Service ()
type LService   = Service ID

instance FromJSON LService where
  parseJSON = withObject "LService" parseLService
    where parseLService obj = Service <$> obj .: "id"
                                      <*> obj .: "type"
                                      <*> obj .: "settings"
                                      <*> obj .: "title"

instance ToJSON a => ToJSON (Service a) where
  toJSON obj = object [ "id"       .= (obj ^. serviceID)
                      , "type"     .= (obj ^. serviceType)
                      , "settings" .= (obj ^. serviceSettings)
                      , "title"    .= (obj ^. serviceTitle) ]

data ChartEntityType = DashboardEntityType |
                       InstrumentEntityType deriving (Show, Eq)

instance FromJSON ChartEntityType where
  parseJSON = withText "ChartEntityType" parseChartEntityType
    where parseChartEntityType "dashboard"  = pure DashboardEntityType
          parseChartEntityType "instrument" = pure InstrumentEntityType
          parseChartEntityType s            = fail $ "unexpected chart entity type " <> unpack s

instance ToJSON ChartEntityType where
  toJSON DashboardEntityType  = "dashboard"
  toJSON InstrumentEntityType = "instrument"

data ChartToken a = ChartToken { _chartTokenID :: a
                               , _chartTokenEntityType :: ChartEntityType
                               , _chartTokenEntityID   :: ID } deriving (Show, Eq)

makeClassy ''ChartToken

type NewChartToken = ChartToken ()
type LChartToken   = ChartToken ID

instance FromJSON LChartToken where
  parseJSON = withObject "LChartToken" parseLChartToken
    where parseLChartToken obj = ChartToken <$> obj .: "token"
                                            <*> obj .: "entity_type"
                                            <*> obj .: "entity_id"

instance ToJSON a => ToJSON (ChartToken a) where
  toJSON obj = object [ "token"       .= (obj ^. chartTokenID)
                      , "entity_type" .= (obj ^. chartTokenEntityType)
                      , "entity_id"   .= (obj ^. chartTokenEntityID) ]

--TODO: fields
data User i p t = User { _userID        :: i
                       , _userEmail     :: Text
                       , _userPassword  :: p
                       , _userAPIToken  :: Text
                       , _userReference :: Text
                       , _userName      :: Maybe Text
                       , _userCompany   :: Maybe Text
                       , _userTimeZone  :: Maybe Text -- see if a stronger type can be used
                       , _userCreatedAt :: t
                       , _userUpdatedAt :: t } deriving (Show, Eq)

makeClassy ''User

type NewUser = User () Text ()
type LUser   = User ID () Text -- see if we can use a datetime

instance FromJSON LUser where
  parseJSON = withObject "LUser" parseLUser
    where parseLUser obj = User <$> obj .:  "id"
                                <*> obj .:  "email"
                                <*> pure ()
                                <*> obj .:  "api_token"
                                <*> obj .:  "reference"
                                <*> obj .:? "name"
                                <*> obj .:? "company"
                                <*> obj .:  "time_zone"
                                <*> obj .:  "created_at"
                                <*> obj .:  "updated_at"

instance (ToJSON i, ToJSON p) => ToJSON (User i p t) where
  toJSON obj = object [ "id"        .= (obj ^. userID)
                      , "email"     .= (obj ^. userEmail)
                      , "password"  .= (obj ^. userPassword)
                      , "api_token" .= (obj ^. userAPIToken)
                      , "reference" .= (obj ^. userReference)
                      , "name"      .= (obj ^. userName)
                      , "company"   .= (obj ^. userCompany)
                      , "time_zone" .= (obj ^. userTimeZone) ]

type LibratoResponse a = Either ErrorDetail a

data ErrorDetail = ParseError Text               |
                   ParamsError [(Text, [Text])]  |
                   RequestError [Text]           |
                   SystemError [Text]            |
                   NotFoundError                 |
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

instance FromJSON (PaginatedResponse LDashboard) where
  parseJSON = parsePaginatedResponse "Dashboard" "dashboards" -- probably

instance FromJSON (PaginatedResponse LInstrument) where
  parseJSON = parsePaginatedResponse "Instrument" "instruments" -- probably

instance FromJSON (PaginatedResponse LService) where
  parseJSON = parsePaginatedResponse "Service" "service" -- probably

instance FromJSON (PaginatedResponse LChartToken) where
  parseJSON = parsePaginatedResponse "ChartToken" "chart_token" -- probably

instance FromJSON (PaginatedResponse LUser) where
  parseJSON = parsePaginatedResponse "User" "user" -- probably

parsePaginatedResponse typeName payloadKey = withObject typeName parseResponse
  where parseResponse obj = PaginatedResponse <$> obj .: "query"
                                              <*> obj .: payloadKey

data PaginatedRequest a = PaginatedRequest {
  _requestPagination :: PaginationOptions
, _requestQuery      :: a
} deriving (Show, Eq)

makeLenses ''PaginatedRequest

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


newtype Metrics = Metrics { _unMetrics :: [Metric] } deriving (Show, Eq)

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

newtype MetricNames = MetricNames {
  _unMetricNames :: [MetricName]
} deriving (Show, Eq)

makeClassy ''MetricNames

instance ToJSON MetricNames where
  toJSON names = object ["names" .= (names ^. unMetricNames)]
