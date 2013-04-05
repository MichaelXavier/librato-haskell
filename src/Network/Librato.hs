{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
module Network.Librato ( getMetrics
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
import System.IO.Streams ( InputStream
                         , fromGenerator
                         , yield
                         , Generator)

import Network.Librato.Types

---- Metrics


getMetrics :: (Monad m, MonadIO m) => PaginatedRequest MetricsSearch -> LibratoM m (InputStream Metric)
getMetrics params = getRequestStreaming "/metrics" params
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

getRequestStreaming :: ( QueryLike query
                       , FromJSON (PaginatedResponse a)
                       , MonadIO m
                       , Monad m)
                       => ByteString
                       -> PaginatedRequest query
                       -> LibratoM m (InputStream a)
getRequestStreaming path params = liftIO $ fromGenerator generator
  where generator = pageGenerator path params

--TODO: eitherT
pageGenerator :: ( QueryLike query
                 , FromJSON (PaginatedResponse a))
                 => ByteString
                 -> PaginatedRequest query
                 -> Generator a ()
pageGenerator path params = do
  Right result <- liftIO $ getPaginatedPage path params
  let meta = result ^. responseMeta
  yieldResults result
  let params' = nextPageParams meta
  unless (atEnd meta) $ pageGenerator path params'
  where atEnd meta = len + offset >= found
          where len    = meta ^. responseLength
                offset = meta ^. responseOffset
                found  = meta ^. responseFound
        yieldResults :: PaginatedResponse a -> Generator a ()
        yieldResults result = mapM_ yield $ result ^. paginationPayload -- theres got to be a traversaal that will do this
        nextPageParams meta = params & requestPagination . offset +~ (meta ^. responseLength)

getPaginatedPage :: ( QueryLike query
                    , Monad m
                    , MonadIO m
                    , FromJSON (PaginatedResponse a))
                    => ByteString
                    -> query
                    -- -> m (LibratoResponse (PaginatedResponse a))
                    -> LibratoM m (LibratoResponse (PaginatedResponse a))
getPaginatedPage = getRequest

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

parseError :: ErrorDetail
parseError = ErrorDetail

setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

getRequest :: ( QueryLike params
              , FromJSON resp
              , MonadIO m
              , Monad m)
              => ByteString
              -> params
              -> LibratoM m (LibratoResponse resp)
getRequest path params = runWithConf =<< S.get
  where runWithConf conf = do
          liftIO $ withLibratoConnection conf $ \conn -> do
            req <- reqFromConf conf path' GET
            sendRequest conn req emptyBody
            receiveResponse conn responseHandler
        path'            = path ++ renderQuery includeQuestion query
        includeQuestion  = True
        query            = toQuery params
