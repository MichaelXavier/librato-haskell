{-# LANGUAGE NoImplicitPrelude    #-}

module Network.Librato ( getMetrics
                       , getAllMetrics
                       , getMetric
                       , createMetric
                       , createMetrics
                       , runLibratoM
                       , deleteMetric
                       , deleteMetrics
                       , getAllDashboards
                       , getDashboards
                       , getDashboard
                       , createDashboard
                       , updateDashboard
                       , deleteDashboard
                       , getAllInstruments
                       , getInstruments
                       , getInstrument
                       , createInstrument
                       , updateInstrument
                       , deleteInstrument
                       , getAllServices
                       , getServices
                       , getService
                       , createService
                       , updateService
                       , deleteService
                       , getAllChartTokens
                       , getChartTokens
                       , getChartToken
                       , createChartToken
                       , updateChartToken
                       , deleteChartToken
                       , getAllUsers
                       , getUsers
                       , getUser
                       , createUser
                       , updateUser
                       , deleteUser
                       , getAllTags
                       , getTags
                       , getTag
                       , tagEntity
                       , deleteTag
                       , getAllAlerts
                       , getAlerts
                       , getAlert
                       , createAlert
                       , updateAlert
                       , deleteAlert
                       , associateServiceWithAlert
                       , disassociateServiceWithAlert
                       , getAllAnnotationStreams
                       , getAnnotationStreams
                       , getAnnotationStream
                       , createAnnotationStream
                       , updateAnnotationStream
                       , deleteAnnotationStream
                       , getAllAnnotationEvents
                       , getAnnotationEvents
                       , getAnnotationEvent
                       , createAnnotationEvent
                       , updateAnnotationEvent
                       , deleteAnnotationEvent
                       , module Network.Librato.Types) where


import ClassyPrelude
import Control.Lens
import qualified Control.Monad.Trans.Reader as R
import qualified System.IO.Streams as S

import Network.Librato.REST
import Network.Librato.Types

-----------------------------
-- Metrics
-----------------------------
getAllMetrics :: PaginatedRequest MetricsSearch -> LibratoM IO (LibratoResponse [Metric])
getAllMetrics = indexResourceAll . MetricResource

getMetrics :: PaginatedRequest MetricsSearch -> LibratoM IO (S.InputStream Metric)
getMetrics = indexResourceStream . MetricResource

getMetric :: MetricLookup -> LibratoM IO (LibratoResponse MetricSummarization)
getMetric = showResource . MetricResource

createMetrics :: [Metric] -> LibratoM IO (LibratoResponse ())
createMetrics = createResource . MetricResource . Metrics

createMetric :: Metric -> LibratoM IO (LibratoResponse ())
createMetric = createMetrics . singleton

deleteMetrics :: [MetricName] -> LibratoM IO (LibratoResponse ())
deleteMetrics = deleteResourceWithBody . MetricResource . MetricNames

deleteMetric :: MetricName -> LibratoM IO (LibratoResponse ())
deleteMetric = deleteResource . MetricResource

--SPECME
updateMetric :: Metric -> LibratoM IO (LibratoResponse ())
updateMetric = updateResource . MetricResource

-----------------------------
-- Dashboards
-----------------------------
getAllDashboards :: PaginatedRequest () -> LibratoM IO (LibratoResponse [LDashboard])
getAllDashboards = indexResourceAll . DashboardResource . unitRequest

getDashboards :: PaginatedRequest () -> LibratoM IO (S.InputStream LDashboard)
getDashboards = indexResourceStream . DashboardResource . unitRequest

getDashboard :: ID -> LibratoM IO (LibratoResponse LDashboard)
getDashboard = showResource . DashboardResource

createDashboard :: NewDashboard -> LibratoM IO (LibratoResponse ())
createDashboard = createResource . DashboardResource

updateDashboard :: LDashboard -> LibratoM IO (LibratoResponse ())
updateDashboard = updateResource . DashboardResource

deleteDashboard :: ID -> LibratoM IO (LibratoResponse ())
deleteDashboard = deleteResource . DashboardResource

-----------------------------
-- Instruments
-----------------------------
getAllInstruments :: PaginatedRequest () -> LibratoM IO (LibratoResponse [LInstrument])
getAllInstruments = indexResourceAll . InstrumentResource . unitRequest

getInstruments :: PaginatedRequest () -> LibratoM IO (S.InputStream LInstrument)
getInstruments = indexResourceStream . InstrumentResource . unitRequest

getInstrument :: ID -> LibratoM IO (LibratoResponse LInstrument)
getInstrument = showResource . InstrumentResource

createInstrument :: NewInstrument -> LibratoM IO (LibratoResponse ())
createInstrument = createResource . InstrumentResource

updateInstrument :: LInstrument -> LibratoM IO (LibratoResponse ())
updateInstrument = updateResource . InstrumentResource

deleteInstrument :: ID -> LibratoM IO (LibratoResponse ())
deleteInstrument = deleteResource . InstrumentResource

-----------------------------
-- Services
-----------------------------
getAllServices :: PaginatedRequest () -> LibratoM IO (LibratoResponse [LService])
getAllServices = indexResourceAll . ServiceResource . unitRequest

getServices :: PaginatedRequest () -> LibratoM IO (S.InputStream LService)
getServices = indexResourceStream . ServiceResource . unitRequest

getService :: ID -> LibratoM IO (LibratoResponse LService)
getService = showResource . ServiceResource

createService :: NewService -> LibratoM IO (LibratoResponse ())
createService = createResource . ServiceResource

updateService :: LService -> LibratoM IO (LibratoResponse ())
updateService = updateResource . ServiceResource

deleteService :: ID -> LibratoM IO (LibratoResponse ())
deleteService = deleteResource . ServiceResource

-----------------------------
-- ChartTokens
-----------------------------
getAllChartTokens :: PaginatedRequest () -> LibratoM IO (LibratoResponse [LChartToken])
getAllChartTokens = indexResourceAll . ChartTokenResource . unitRequest

getChartTokens :: PaginatedRequest () -> LibratoM IO (S.InputStream LChartToken)
getChartTokens = indexResourceStream . ChartTokenResource . unitRequest

getChartToken :: ID -> LibratoM IO (LibratoResponse LChartToken)
getChartToken = showResource . ChartTokenResource

createChartToken :: NewChartToken -> LibratoM IO (LibratoResponse ())
createChartToken = createResource . ChartTokenResource

updateChartToken :: LChartToken -> LibratoM IO (LibratoResponse ())
updateChartToken = updateResource . ChartTokenResource

deleteChartToken :: ID -> LibratoM IO (LibratoResponse ())
deleteChartToken = deleteResource . ChartTokenResource

-----------------------------
-- Users
-----------------------------
getAllUsers :: PaginatedRequest () -> LibratoM IO (LibratoResponse [LUser])
getAllUsers = indexResourceAll . UserResource . unitRequest

getUsers :: PaginatedRequest () -> LibratoM IO (S.InputStream LUser)
getUsers = indexResourceStream . UserResource . unitRequest

getUser :: ID -> LibratoM IO (LibratoResponse LUser)
getUser = showResource . UserResource

createUser :: NewUser -> LibratoM IO (LibratoResponse ())
createUser = createResource . UserResource

updateUser :: LUser -> LibratoM IO (LibratoResponse ())
updateUser = updateResource . UserResource

deleteUser :: ID -> LibratoM IO (LibratoResponse ())
deleteUser = deleteResource . UserResource

-----------------------------
-- Tags
-----------------------------
getAllTags :: PaginatedRequest () -> LibratoM IO (LibratoResponse [Tag])
getAllTags = indexResourceAll . TagResource . unitRequest

getTags :: PaginatedRequest () -> LibratoM IO (S.InputStream Tag)
getTags = indexResourceStream . TagResource . unitRequest

getTag :: TagName -> LibratoM IO (LibratoResponse Tag)
getTag = showResource . TagResource

tagEntity :: TagName -> TaggedEntity -> LibratoM IO (LibratoResponse ())
tagEntity = curry (createResource . TagResource)

deleteTag :: TagName -> LibratoM IO (LibratoResponse ())
deleteTag = deleteResource . TagResource

-----------------------------
-- Alerts
-----------------------------
getAllAlerts :: PaginatedRequest AlertSearch -> LibratoM IO (LibratoResponse [LAlert])
getAllAlerts = indexResourceAll . AlertResource

getAlerts :: PaginatedRequest AlertSearch -> LibratoM IO (S.InputStream LAlert)
getAlerts = indexResourceStream . AlertResource

getAlert :: ID -> LibratoM IO (LibratoResponse LAlert)
getAlert = showResource . AlertResource

createAlert :: NewAlert -> LibratoM IO (LibratoResponse ())
createAlert = createResource . AlertResource

updateAlert :: LAlert -> LibratoM IO (LibratoResponse ())
updateAlert = updateResource . AlertResource

deleteAlert :: ID -> LibratoM IO (LibratoResponse ())
deleteAlert = deleteResource . AlertResource

associateServiceWithAlert :: AlertServiceAssociation -> LibratoM IO (LibratoResponse ())
associateServiceWithAlert = createResource . ASAResource

disassociateServiceWithAlert :: AlertServiceAssociation -> LibratoM IO (LibratoResponse ())
disassociateServiceWithAlert = deleteResource . ASAResource

-----------------------------
-- AnnotationStreams
-----------------------------
--TODO: ordering by name in pagination
getAllAnnotationStreams :: PaginatedRequest () -> LibratoM IO (LibratoResponse [AnnotationStream])
getAllAnnotationStreams = indexResourceAll . AnnotationStreamResource

getAnnotationStreams :: PaginatedRequest () -> LibratoM IO (S.InputStream AnnotationStream)
getAnnotationStreams = indexResourceStream . AnnotationStreamResource

getAnnotationStream :: ASName -> LibratoM IO (LibratoResponse AnnotationStream)
getAnnotationStream = showResource . AnnotationStreamResource

createAnnotationStream :: AnnotationStream -> LibratoM IO (LibratoResponse ())
createAnnotationStream = createResource . AnnotationStreamResource

updateAnnotationStream :: AnnotationStream -> LibratoM IO (LibratoResponse ())
updateAnnotationStream = updateResource . AnnotationStreamResource

deleteAnnotationStream :: ASName -> LibratoM IO (LibratoResponse ())
deleteAnnotationStream = deleteResource . AnnotationStreamResource

-----------------------------
-- AnnotationEvents
-----------------------------
--TODO: ordering by name in pagination
getAllAnnotationEvents :: PaginatedRequest ASName -> LibratoM IO (LibratoResponse [LAnnotationEvent])
getAllAnnotationEvents = undefined

getAnnotationEvents :: PaginatedRequest ASName -> LibratoM IO (S.InputStream LAnnotationEvent)
getAnnotationEvents = undefined

getAnnotationEvent :: ASName -> ID -> LibratoM IO (LibratoResponse LAnnotationEvent)
getAnnotationEvent name eid = undefined

createAnnotationEvent :: ASName -> NewAnnotationEvent -> LibratoM IO (LibratoResponse ())
createAnnotationEvent = undefined

updateAnnotationEvent :: ASName -> LAnnotationEvent -> LibratoM IO (LibratoResponse ())
updateAnnotationEvent = undefined

deleteAnnotationEvent :: ASName -> ID -> LibratoM IO (LibratoResponse ())
deleteAnnotationEvent name = undefined

-----------------------------
-- LibratoM tools
-----------------------------
runLibratoM :: Monad m => ClientConfiguration -> LibratoM m a -> m a
runLibratoM = flip R.runReaderT

-----------------------------
-- Helpers
-----------------------------
unitRequest :: PaginatedRequest () -> PaginatedRequest Unit
unitRequest q = q & requestQuery .~ Unit
