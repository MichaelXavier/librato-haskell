{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Network.Librato ( getMetrics
                       , getAllMetrics
                       , createMetric
                       , createMetrics
                       , runLibratoM
                       , module Network.Librato.Types) where

import Blaze.ByteString.Builder (Builder)
import ClassyPrelude
import Control.Exception (throw)
import Control.Lens
import Control.Monad ((<=<))
import qualified Control.Monad.Trans.Reader as R
import Data.Default
import Data.Ix (inRange)
import Data.Aeson ( FromJSON
                  , fromJSON
                  , ToJSON(..)
                  , Result(..)
                  , encode
                  , json)
import Debug.Trace (traceShow)
import OpenSSL (withOpenSSL)
import Network.Http.Client ( sendRequest
                           --, emptyBody
                           , receiveResponse
                           , buildRequest
                           , Response
                           , RequestBuilder
                           , Method(..)
                           , Connection
                           , inputStreamBody
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
getMetrics = getJSONRequestStreaming "/metrics"

--
---- TODO: flesh out
--data MetricLookup = MetricLookup deriving (Show, Eq)
--
--getMetric :: MetricLookup -> LibratoM (LibratoResponse (Maybe Metric))
--getMetric = undefined

createMetric :: Metric -> LibratoM IO (LibratoResponse ())
createMetric = createMetrics . singleton

createMetrics :: [Metric] -> LibratoM IO (LibratoResponse ())
createMetrics = postJSON_ "/metrics" . Metrics

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

getJSONRequestStreaming :: ( QueryLike query
                           , FromJSON (PaginatedResponse a))
                           => ByteString
                           -> PaginatedRequest query
                           -> LibratoM IO (S.InputStream a)
getJSONRequestStreaming path params = do
  conf <- R.ask 
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
                    -> IO (LibratoResponse (PaginatedResponse a))
getPaginatedPage = getJSONRequest


setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

--TODO: steal redirect following logic from "get" convenience function
getJSONRequest :: ( QueryLike params
              , FromJSON resp)
              => ClientConfiguration
              -> ByteString
              -> params
              -> IO (LibratoResponse resp)
getJSONRequest conf path params = do input <- noBody
                                     executeRequest input GET conf path'
  where path'            = path ++ renderQuery includeQuestion query
        includeQuestion  = True
        query            = toQuery params

postJSON_ :: ToJSON a => ByteString -> a -> LibratoM IO (LibratoResponse ())
postJSON_ = sendJSONBody_ POST

putJSON_ :: ToJSON a => ByteString -> a -> LibratoM IO (LibratoResponse ())
putJSON_ = sendJSONBody_ PUT

sendJSONBody_ :: ToJSON a => Method -> ByteString -> a -> LibratoM IO (LibratoResponse ())
sendJSONBody_ meth path payload = do 
  conf          <- R.ask
  liftIO $ do payloadStream <- jsonToInputStream payload
              executeRequest_ payloadStream meth conf path

executeRequest :: (FromJSON resp)
                  => (S.InputStream ByteString)
                  -> Method
                  -> ClientConfiguration
                  -> ByteString
                  -> IO (LibratoResponse resp)
executeRequest = executeRequestWithHandler jsonResponseHandler

executeRequest_ :: (S.InputStream ByteString)
                   -> Method
                   -> ClientConfiguration
                   -> ByteString
                   -> IO (LibratoResponse ())
executeRequest_ = executeRequestWithHandler emptyResponseHandler

executeRequestWithHandler :: (Response -> S.InputStream ByteString -> IO (LibratoResponse a))
                             -> S.InputStream ByteString
                             -> Method
                             -> ClientConfiguration
                             -> ByteString
                             -> IO (LibratoResponse a)
executeRequestWithHandler handler requestBodyStream meth conf path = do
  liftIO $ withLibratoConnection conf $ \conn -> do
    req <- reqFromConf conf path meth
    sendRequest conn req (inputStreamBody requestBodyStream)
    receiveResponse conn handler

jsonToInputStream :: ToJSON a => a -> IO (S.InputStream ByteString)
jsonToInputStream = S.fromLazyByteString . encode . toJSON

noBody :: IO (S.InputStream ByteString)
noBody = S.nullInput

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


jsonResponseHandler :: (FromJSON resp)
                       => Response
                       -> S.InputStream ByteString
                       -> IO (LibratoResponse resp)
jsonResponseHandler = responseHandler handleJSONBody
  where handleJSONBody :: FromJSON resp => S.InputStream ByteString -> IO (LibratoResponse resp)
        handleJSONBody stream = do parsed <- parseJSONBody stream
                                   return $ coerceParsed parsed

-- will const come and bite me in the ass? maybe `seq` ()
emptyResponseHandler = responseHandler (const . return . Right $ ())

-- I shoudln't have to specify the FromJSON
responseHandler :: (FromJSON a)
                   => (S.InputStream ByteString -> IO (LibratoResponse a))
                   -> Response
                   -> S.InputStream ByteString
                   -> IO (LibratoResponse a)
responseHandler bodyHandler resp stream
  | responseOk    = bodyHandler stream -- do we need IO here?
  | unauthorized  = returnError UnauthorizedError
  | alreadyExists = returnError UnauthorizedError
  | maintenance   = returnError MaintenanceError
  | otherwise = undefined
  -- | otherwise     = do parsed <- parseJSONBody stream
  --                      case parsed of
  --                        Success err -> returnError OtherError --TODO
  --                        Error e     -> returnError $ ParseError $ pack e
  where responseOk    = inRange (200, 299) $ getStatusCode resp
        unauthorized  = 401 == getStatusCode resp
        alreadyExists = 422 == getStatusCode resp
        maintenance   = 503 == getStatusCode resp
        returnError   = return . Left
        --TODO: catch goddamn ParseExceptions, make this less horrible

coerceParsed :: Result a -> LibratoResponse a
coerceParsed (Success a) = Right a
coerceParsed (Error e)   = Left $ ParseError $ pack e

--TODO: pointfree, typesig
parseJSONBody :: FromJSON a => S.InputStream ByteString -> IO (Result a)
parseJSONBody stream = parseFromStream parser stream
  where parser = fmap fromJSON json
