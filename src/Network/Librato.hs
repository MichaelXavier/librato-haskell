{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Network.Librato ( getMetrics
                       , getAllMetrics
                       , runLibratoM
                       , module Network.Librato.Types) where

import ClassyPrelude
import Control.Exception (throw)
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
                           , openConnection
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
getAllMetrics :: PaginatedRequest MetricsSearch -> LibratoM IO (LibratoResponse [Metric])
getAllMetrics = consumeStreamingCall . getMetrics

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

--TODO: cleanup
consumeStreamingCall :: LibratoM IO (S.InputStream a) -> LibratoM IO (LibratoResponse [a])
consumeStreamingCall req = withErrorHandling consume
  where consume = do stream <- req
                     xs <- liftIO $ S.toList stream
                     return $ Right xs
        withErrorHandling = catch' handleError
        handleError :: ErrorDetail -> LibratoM IO (LibratoResponse [b])
        handleError ed = return $ Left ed
        catch' = flip catch

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
pageGenerator conf path params = do eResult <- getPage
                                    case eResult of
                                      Right result -> keepGoing result
                                      Left e       -> throw e
  where atEnd meta = len + offset >= found
          where len    = meta ^. responseLength
                offset = meta ^. responseOffset
                found  = meta ^. responseFound
        yieldResults result = mapM_ S.yield $ result ^. paginationPayload -- theres got to be a traversaal that will do this
        nextPageParams meta = params & requestPagination . offset +~ (meta ^. responseLength)
        getPage = liftIO $ getPaginatedPage conf path params
        keepGoing result = do yieldResults result
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
            sendRequest conn req emptyBody
            receiveResponse conn responseHandler
        path'            = path ++ renderQuery includeQuestion query
        includeQuestion  = True
        query            = toQuery params

withLibratoConnection :: ClientConfiguration -> (Connection -> IO a) -> IO a
withLibratoConnection conf action = withConnection establishConnection action
  where host = conf ^. apiHostname
        port = conf ^. apiPort
        establishConnection
          | conf ^. apiUseSSL = establishConnectionWithoutSSL
          | otherwise      = establishConnectionWithoutSSL
        establishConnectionWithoutSSL = openConnection host port
        establishConnectionWithSSL    = withOpenSSL $ do ctx <- baselineContextSSL
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
  | responseOk   = do parsed <- parseBody
                      return $ coerceParsed parsed
  | unauthorized  = returnError UnauthorizedError
  | alreadyExists = returnError UnauthorizedError
  | maintenance   = returnError MaintenanceError
  | otherwise     = do parsed <- parseBody
                       case parsed of
                         Success err -> return $ Left OtherError --TODO
                         Error e     -> return $ Left $ ParseError $ pack e
  where responseOk    = inRange (200, 299) $ getStatusCode resp
        unauthorized  = 401 == getStatusCode resp
        alreadyExists = 422 == getStatusCode resp
        maintenance   = 503 == getStatusCode resp
        parseBody     = parseFromStream parser stream
        parser        = fmap fromJSON json
        returnError   = return . Left
        --TODO: catch goddamn ParseExceptions, make this less horrible

coerceParsed :: Result a -> LibratoResponse a
coerceParsed (Success a) = Right a
coerceParsed (Error e)   = Left $ ParseError $ pack e
