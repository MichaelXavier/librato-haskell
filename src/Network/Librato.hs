{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Network.Librato ( MetricsSearch(..)
                       , HasMetricsSearch(..)
                       , module Network.Librato.Types) where

import ClassyPrelude
import Control.Lens
import qualified Control.Monad.Trans.State as S
import Data.Default
import Data.Ix (inRange)
import Data.Aeson ( FromJSON
                  , fromJSON
                  , Result(..)
                  , json)
import Network.Http.Client ( sendRequest
                           , emptyBody
                           , receiveResponse
                           , buildRequest
                           , Response
                           , RequestBuilder
                           , Method(..)
                           , Connection
                           , Request
                           , withConnection
                           , getStatusCode
                           , openConnection
                           , http
                           , setContentType
                           , setAccept
                           , setAuthorizationBasic
                           , setHeader)
import Network.HTTP.Types ( QueryLike(..)
                          , renderQuery)
import System.IO.Streams.Attoparsec (parseFromStream)
import System.IO.Streams (InputStream)

import Network.Librato.Types

---- Metrics

data MetricsSearch = MetricsSearch { _metricsNamed            :: Maybe Text -- case insensitive
                                   , _metricsSearchPagination :: PaginationOptions
                                   , _metricsSearchTags       :: [Tag] } deriving (Show, Eq)

makeClassy ''MetricsSearch

--instance HasPagination MetricsSearch where
--  pagination = _metricsSearchPagination

instance Default MetricsSearch where
  def = MetricsSearch Nothing def empty

instance QueryLike MetricsSearch where
  toQuery ms = tagQueries ++ maybeToList nameQuery
    where nameQuery       = ("name", ) . Just . encodeUtf8 <$> ms ^. metricsNamed
          tagQueries      = map toTagQuery $ ms ^. metricsSearchTags
          toTagQuery      = ("tags[]",) . Just . encodeUtf8 . _tagName

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
getRequest' :: ( QueryLike params
               , FromJSON resp
               , MonadIO m
               , Monad m)
              => params
              -> ByteString
              -> LibratoM m (LibratoResponse resp)
getRequest' params path = runWithConf =<< S.get
  where runWithConf conf = do
          liftIO $ withLibratoConnection conf $ \conn -> do
            req <- reqFromConf conf path' GET
            sendRequest conn req emptyBody
            receiveResponse conn responseHandler
        path'            = path ++ renderQuery includeQuestion query
        includeQuestion  = True
        query            = toQuery params

--withLibratoConnection :: ClientConfiguration -> (Connection -> a) -> a
withLibratoConnection conf action = withConnection (openConnection host port) action
  where host = conf ^. apiHostname
        port = conf ^. apiPort


responseHandler :: (FromJSON parsed)
                   => Response
                   -> InputStream ByteString
                   -> IO (LibratoResponse parsed)
responseHandler resp stream
  | responseOk = do parsed <- parseBody
                    case parsed of
                      Success a -> return $ Right a
                      _         -> return $ Left parseError
  | otherwise  = do parsed <- parseBody
                    case parsed of
                      Success err -> return $ Left ErrorDetail --todo
                      _           -> return $ Left parseError
  where responseOk = inRange (200, 299) $ getStatusCode resp
        parseBody  = parseFromStream parser stream
        parser     = fmap fromJSON json
        --TODO: catch goddamn ParseExceptions, make this less horrible

parseError :: ErrorDetail
parseError = ErrorDetail

--TODO: EitherT
reqFromConf :: ClientConfiguration -> ByteString -> Method -> IO Request
reqFromConf conf path meth = buildRequest $ do
  http meth fullPath
  setContentType "application/json"
  setAccept      "application/json"
  setAuthorizationBasic user token
  setUserAgent ua
  where fullPath = conf ^. apiBasePath ++ path
        user     = conf ^. apiUser
        token    = conf ^. apiToken
        ua       = conf ^. apiUserAgent

setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

--getRequestStreaming :: ( QueryLike params
--                       , HasPagination params
--                       , FromJSON resp
--                       , MonadIO m
--                       , Monad m)
--                       => params
--                       -> ByteString
--                       -> (resp -> [a])
--                       -> LibratoM m (InputStream a)
--getRequestStreaming params path unwrap = fromGenerator generator
--  where generator = pageGenerator params path unwrap
--
--pageGenerator :: ( QueryLike params
--                 , HasPagination params
--                 , FromJSON resp
--                 , MonadIO m
--                 , Monad m)
--                 => params
--                 -> ByteString
--                 -> (resp -> [a])
--                 -> LibratoM m (Generator a ())
--pageGenerator params path unwrap = getEachPage
