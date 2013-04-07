{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleContexts     #-}
module Network.Librato ( getMetrics
                       , getAllMetrics
                       , runLibratoM
                       , module Network.Librato.Types) where

import ClassyPrelude
import Control.Lens
import Control.Monad ((<=<))
import qualified Control.Monad.Trans.Reader as R
import Data.Default
import Data.Ix (inRange)
import Data.Aeson ( FromJSON
                  , fromJSON
                  , Result(..)
                  , json)
import Debug.Trace (traceShow)
import OpenSSL (withOpenSSL)
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
                           , openConnectionSSL
                           , baselineContextSSL
                           , http
                           , setContentType
                           , setAccept
                           , setAuthorizationBasic
                           , setHeader)
import Network.HTTP.Types ( QueryLike(..)
                          , renderQuery)
import System.IO.Streams.Attoparsec (parseFromStream)
import qualified System.IO.Streams as S

import Network.Librato.Types

---- Metrics

--TODO: monadic composition
getAllMetrics :: PaginatedRequest MetricsSearch -> LibratoM IO [Metric]
getAllMetrics = liftIO . S.toList <=< getMetrics

getMetrics :: PaginatedRequest MetricsSearch -> LibratoM IO (S.InputStream Metric)
getMetrics = getRequestStreaming "/metrics"
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

runLibratoM :: Monad m => ClientConfiguration -> LibratoM m a -> m a
runLibratoM = flip R.runReaderT


getRequestStreaming :: ( QueryLike query
                       , FromJSON (PaginatedResponse a))
                       => ByteString
                       -> PaginatedRequest query
                       -> LibratoM IO (S.InputStream a)
getRequestStreaming path params = do conf <- R.ask 
                                     let gen = pageGenerator conf path params
                                     liftIO $ S.fromGenerator gen

--TODO: eitherT
pageGenerator :: ( QueryLike query
                 , FromJSON (PaginatedResponse a))
                 => ClientConfiguration
                 -> ByteString
                 -> PaginatedRequest query
                 -> S.Generator a () -- is this IO needed? see if we can strip it
pageGenerator conf path params = runDat
  where atEnd meta = len + offset >= found
          where len    = meta ^. responseLength
                offset = meta ^. responseOffset
                found  = meta ^. responseFound
        yieldResults :: PaginatedResponse a -> S.Generator a ()
        yieldResults result = mapM_ S.yield $ result ^. paginationPayload -- theres got to be a traversaal that will do this
        nextPageParams meta = params & requestPagination . offset +~ (meta ^. responseLength)
        runDat = do Right result <- liftIO $ getPaginatedPage conf path params -- FIXME
                    yieldResults result -- need some sort of lift there
                    let meta = result ^. responseMeta
                    let params' = nextPageParams meta
                    --unless (atEnd meta) $ pageGenerator path params'
                    pageGenerator conf path params'
                    return ()


getPaginatedPage :: ( QueryLike query
                    , FromJSON (PaginatedResponse a))
                    => ClientConfiguration
                    -> ByteString
                    -> query
                    -- -> m (LibratoResponse (PaginatedResponse a))
                    -> IO (LibratoResponse (PaginatedResponse a))
getPaginatedPage = getRequest


parseError :: ErrorDetail
parseError = ErrorDetail

setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

--TODO: steal redirect following logic from "get" convenience function
getRequest :: ( QueryLike params
              , FromJSON resp)
              => ClientConfiguration
              -> ByteString
              -> params
              -> IO (LibratoResponse resp)
getRequest conf path params = runWithConf
  where runWithConf = do
          liftIO $ withLibratoConnection conf $ \conn -> do
            req <- reqFromConf conf path' GET
            print req
            sendRequest conn req emptyBody
            receiveResponse conn responseHandler
        path'            = path ++ renderQuery includeQuestion query
        includeQuestion  = True
        query            = toQuery params

withLibratoConnection :: ClientConfiguration -> (Connection -> IO a) -> IO a
withLibratoConnection conf action = withConnection establishConnection action
  where host = conf ^. apiHostname
        port = conf ^. apiPort
        establishConnection = withOpenSSL $ do ctx <- baselineContextSSL
                                               openConnectionSSL ctx host port

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

responseHandler :: (FromJSON parsed)
                   => Response
                   -> S.InputStream ByteString
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
        parseBody  = traceShow resp $ parseFromStream parser stream
        parser     = fmap fromJSON json
        --TODO: catch goddamn ParseExceptions, make this less horrible
