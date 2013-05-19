{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Librato.REST ( indexResourceAll
                            , indexResourceStream
                            , showResource
                            , createResource
                            , deleteResource
                            , deleteResourceWithBody
                            , updateResource
                            , MetricResource(..)
                            , DashboardResource(..)) where

import ClassyPrelude
import Control.Exception (throw)
import Control.Lens
import qualified Control.Monad.Trans.Reader as R
import Data.Aeson ( FromJSON
                  , fromJSON
                  , ToJSON(..)
                  , Result(..)
                  , encode
                  , json)
import Data.Ix (inRange)
import OpenSSL (withOpenSSL)
import Network.Http.Client ( sendRequest
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

-------------------------------
-- Typeclasses
-------------------------------
class PayloadWithID t where
  payloadID :: t -> ByteString

class NamedResource r where
  resourceName :: r -> ByteString

-- may not need to be a lens, maybe could be a getter
class PayloadResource r a | r -> a where
  resourcePayload :: Lens' r a

-------------------------------
-- Metrics
-------------------------------
newtype MetricResource a = MetricResource { _metricResourcePayload :: a }

makeClassy ''MetricResource

instance NamedResource (MetricResource a) where
  resourceName = const "metrics"

instance PayloadResource (MetricResource a) a where
  resourcePayload = metricResourcePayload

instance PayloadWithID MetricLookup where
  payloadID = encodeUtf8 . view metricLookupName

instance PayloadWithID MetricName where
  payloadID = encodeUtf8 . view unMetricName

instance PayloadWithID Metric where
  payloadID = payloadID . view metricName

-------------------------------
-- Dashboard
-------------------------------
newtype DashboardResource a = DashboardResource { _dashboardResourcePayload :: a }

makeClassy ''DashboardResource

instance NamedResource (DashboardResource a) where
  resourceName = const "dashboards"

instance PayloadResource (DashboardResource a) a where
  resourcePayload = dashboardResourcePayload

instance PayloadWithID LDashboard where
  payloadID = payloadID . view dashboardID

-------------------------------
-- Generally applicable instances
-------------------------------
instance QueryLike Unit where
  toQuery = const []

--TODO: killme
instance QueryLike () where
  toQuery = const []

instance PayloadWithID ID where
  payloadID = encodeUtf8 . view unID

instance QueryLike ID where
  toQuery = const []

-------------------------------
-- REST calls
-------------------------------
indexResourceAll :: ( QueryLike q
                    , NamedResource r
                    , PayloadResource r (PaginatedRequest q)
                    , FromJSON (PaginatedResponse a))
                    => r
                    -> LibratoM IO (LibratoResponse [a])
indexResourceAll = consumeStreamingCall . indexResourceStream

indexResourceStream :: ( QueryLike q
                       , NamedResource r
                       , PayloadResource r (PaginatedRequest q)
                       , FromJSON (PaginatedResponse a))
                       => r
                       -> LibratoM IO (S.InputStream a)
indexResourceStream resource = getJSONRequestStreaming path params
  where path   = indexPath resource
        params = resource ^. resourcePayload

-- not found will return a Left
showResource :: ( QueryLike q
                , NamedResource r
                , PayloadResource r q
                , PayloadWithID q
                , FromJSON a)
                => r
                -> LibratoM IO (LibratoResponse a)
showResource resource = do conf <- R.ask
                           liftIO $ getJSONRequest conf path params
  where path   = existingPath resource
        params = resource ^. resourcePayload

--TODO: return ID or a resource with id? do we care?
createResource :: ( NamedResource r
                  , PayloadResource r p
                  , ToJSON p)
                  => r
                  -> LibratoM IO (LibratoResponse ())
createResource resource = postJSON_ path payload
  where path    = indexPath resource
        payload = resource ^. resourcePayload

updateResource :: ( NamedResource r
                  , PayloadResource r p
                  , PayloadWithID p
                  , ToJSON p)
                  => r
                  -> LibratoM IO (LibratoResponse ())
updateResource resource = putJSON_ path payload
  where path    = existingPath resource
        payload = resource ^. resourcePayload

deleteResourceWithBody :: ( NamedResource r
                          , PayloadResource r p
                          , ToJSON p)
                          => r
                          -> LibratoM IO (LibratoResponse ())
deleteResourceWithBody resource = deleteJSON_ path payload
  where path    = indexPath resource
        payload = resource ^. resourcePayload

deleteResource :: ( NamedResource r
                  , PayloadResource r q
                  , PayloadWithID q)
                  => r
                  -> LibratoM IO (LibratoResponse ())
deleteResource resource = delete_ path
  where path = existingPath resource

-------------------------------
-- Plumbing
-------------------------------
indexPath :: NamedResource r => r -> ByteString
indexPath = ("/" <>) . resourceName

existingPath :: ( NamedResource r
                , PayloadResource r p
                , PayloadWithID p)
                => r
                -> ByteString
existingPath resource = indexPath resource <> "/" <> rid
  where rid = resource ^. (resourcePayload . to payloadID)

-- fix up this nasty shit
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

noBody :: IO (S.InputStream ByteString)
noBody = S.nullInput

executeRequest :: (FromJSON resp)
                  => (S.InputStream ByteString)
                  -> Method
                  -> ClientConfiguration
                  -> ByteString
                  -> IO (LibratoResponse resp)
executeRequest = executeRequestWithHandler jsonResponseHandler False

executeRequestWithHandler :: (Response -> S.InputStream ByteString -> IO (LibratoResponse a))
                             -> Bool --HACK
                             -> S.InputStream ByteString
                             -> Method
                             -> ClientConfiguration
                             -> ByteString
                             -> IO (LibratoResponse a)
executeRequestWithHandler handler closeConn requestBodyStream meth conf path = do
  liftIO $ withLibratoConnection conf $ \conn -> do
    req <- reqFromConf conf path meth closeConn
    sendRequest conn req (inputStreamBody requestBodyStream)
    receiveResponse conn handler

jsonResponseHandler :: (FromJSON resp)
                       => Response
                       -> S.InputStream ByteString
                       -> IO (LibratoResponse resp)
jsonResponseHandler = responseHandler handleJSONBody
  where handleJSONBody :: FromJSON resp => S.InputStream ByteString -> IO (LibratoResponse resp)
        handleJSONBody stream = do parsed <- parseJSONBody stream
                                   return $ coerceParsed parsed

reqFromConf :: ClientConfiguration -> ByteString -> Method -> Bool -> IO Request
reqFromConf conf path meth closeConn = buildRequest $ do
  http meth fullPath
  setContentType "application/json"
  setAccept      "application/json"
  setAuthorizationBasic user token
  setUserAgent ua
  where fullPath = conf ^. apiBasePath ++ path
        user     = conf ^. apiUser
        token    = conf ^. apiToken
        ua       = conf ^. apiUserAgent

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
  | notFound      = returnError NotFoundError
  | otherwise     = undefined --TODO
  -- | otherwise     = do parsed <- parseJSONBody stream
  --                      case parsed of
  --                        Success err -> returnError OtherError --TODO
  --                        Error e     -> returnError $ ParseError $ pack e
  where responseOk    = inRange (200, 299) $ getStatusCode resp
        unauthorized  = 401 == getStatusCode resp
        alreadyExists = 422 == getStatusCode resp
        maintenance   = 503 == getStatusCode resp
        notFound      = 404 == getStatusCode resp
        returnError   = return . Left
        --TODO: catch goddamn ParseExceptions, make this less horrible

coerceParsed :: Result a -> LibratoResponse a
coerceParsed (Success a) = Right a
coerceParsed (Error e)   = Left $ ParseError $ pack e

--TODO: pointfree, typesig
parseJSONBody :: FromJSON a => S.InputStream ByteString -> IO (Result a)
parseJSONBody stream = parseFromStream parser stream
  where parser = fmap fromJSON json

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

postJSON_ :: ToJSON a => ByteString -> a -> LibratoM IO (LibratoResponse ())
postJSON_ = sendJSONBody_ POST

putJSON_ :: ToJSON a => ByteString -> a -> LibratoM IO (LibratoResponse ())
putJSON_ = sendJSONBody_ PUT

deleteJSON_ :: ToJSON a => ByteString -> a -> LibratoM IO (LibratoResponse ())
deleteJSON_ = sendJSONBody_ DELETE

delete_ :: ByteString -> LibratoM IO (LibratoResponse ())
delete_ path = do input <- liftIO $ noBody
                  conf  <- R.ask 
                  liftIO $ executeRequest_ input DELETE conf path

sendJSONBody_ :: ToJSON a => Method -> ByteString -> a -> LibratoM IO (LibratoResponse ())
sendJSONBody_ meth path payload = do 
  conf          <- R.ask
  liftIO $ do payloadStream <- jsonToInputStream payload
              executeRequest_ payloadStream meth conf path

jsonToInputStream :: ToJSON a => a -> IO (S.InputStream ByteString)
jsonToInputStream = S.fromLazyByteString . encode . toJSON

executeRequest_ :: (S.InputStream ByteString)
                   -> Method
                   -> ClientConfiguration
                   -> ByteString
                   -> IO (LibratoResponse ())
executeRequest_ = executeRequestWithHandler emptyResponseHandler True

emptyResponseHandler :: Response -> S.InputStream ByteString -> IO (LibratoResponse ())
emptyResponseHandler = responseHandler (const . return . Right $ ())
