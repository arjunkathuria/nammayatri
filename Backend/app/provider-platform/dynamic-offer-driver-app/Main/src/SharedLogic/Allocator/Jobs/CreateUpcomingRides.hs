module SharedLogic.Allocator.Jobs.CreateUpcomingRides where

import Data.Time.LocalTime
import qualified Domain.Types.TimeRange as TimeRange
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.Queries.AllocatorJob as QAllocatorJob
import qualified Storage.Queries.Timetable as QTimetable

getCurrentLocalTime :: MonadTime m => TimeZone -> m LocalTime
getCurrentLocalTime tz = do
  now <- getCurrentTime
  pure $ utcToLocalTime tz now

handle :: (Redis.HedisFlow m r, EsqDBFlow m r) => m ()
handle = Redis.whenWithLockRedis "jobs:createUpcomingRides" 60 $ do
  timeZone <- liftIO getCurrentTimeZone
  now <- getCurrentLocalTime timeZone
  let oneHour = 60 * 60
      timeRange = TimeRange.fromTimeAndDuration now oneHour
  Esq.runTransaction $ do
    timetables <- QTimetable.findAllUnscheduledAndActiveDuring timeRange
    QAllocatorJob.createUpcomingRideJobs timetables
