{-# LANGUAGE NoImplicitPrelude    #-}
module Network.Librato ( getMetrics
                       , getAllMetrics
                       , getMetric
                       , createMetric
                       , createMetrics
                       , runLibratoM
                       , deleteMetric
                       , deleteMetrics
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
--getAllDashboards :: LibratoM IO (LibratoResponse [Dashboard])
--getAllDashboards = undefined
--
--getDashboards :: LibratoM IO (S.InputStream Dashboard)
--getDashboards = undefined
--
----TODO: newtype to DashboardID
----TODO: should this even be Maybe
--getDashboard :: Text -> LibratoM IO (Maybe Dashboard)
--getDashboard = undefined
--
----TODO: new instrument has no id
--createDashboard :: NewDashboard -> LibratoM IO (LibratoResponse Dashboard)
--createDashboard = undefined
--
--updateDashboard :: Dashboard -> LibratoM IO (LibratoResponse Dashboard)
--updateDashboard = undefined
--
----TODO: newtype to DashboardID
--deleteDashboard :: Text -> LibratoM IO (LibratoResponse ())
--deleteDashboard = undefined

-----------------------------
-- Instruments
-----------------------------
--getAllInstruments :: LibratoM IO (LibratoResponse [Instrument])
--getAllInstruments = undefined
--
--getInstruments :: LibratoM IO (S.InputStream Instrument)
--getInstruments = undefined
--
----TODO: newtype to InstrumentID
----TODO: should this even be Maybe
--getInstrument :: Text -> LibratoM IO (Maybe Instrument)
--getInstrument = undefined
--
----TODO: new instrument has no id
--createInstrument :: NewInstrument -> LibratoM IO (LibratoResponse Instrument)
--createInstrument = undefined
--
--updateInstrument :: Instrument -> LibratoM IO (LibratoResponse Instrument)
--updateInstrument = undefined
--
----TODO: newtype to InstrumentID
--deleteInstrument :: Text -> LibratoM IO (LibratoResponse ())
--deleteInstrument = undefined

-----------------------------
-- LibratoM tools
-----------------------------
runLibratoM :: Monad m => ClientConfiguration -> LibratoM m a -> m a
runLibratoM = flip R.runReaderT
