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
deleteMetrics = deleteResourceWithBody . MetricResource

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
-- LibratoM tools
-----------------------------
runLibratoM :: Monad m => ClientConfiguration -> LibratoM m a -> m a
runLibratoM = flip R.runReaderT

-----------------------------
-- Helpers
-----------------------------
unitRequest :: PaginatedRequest () -> PaginatedRequest Unit
unitRequest q = q & requestQuery .~ Unit
