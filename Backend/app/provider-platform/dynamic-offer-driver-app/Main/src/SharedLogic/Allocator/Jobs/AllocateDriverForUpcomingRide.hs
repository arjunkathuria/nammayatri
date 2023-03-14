module SharedLogic.Allocator.Jobs.AllocateDriverForUpcomingRide where

import qualified Control.Monad.Catch as Exception
import Data.Time.LocalTime
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.Booking.BookingLocation as Booking
import qualified Domain.Types.DriverQuote as DDriverQuote
import qualified Domain.Types.FarePolicy.FarePolicy as FarePolicy
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSearchReq
import qualified Domain.Types.Timetable as Timetable
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..), SendSearchRequestToDriverJobData (..))
import SharedLogic.FareCalculator (calculateFare, fareSum)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.AllocatorJob as QAllocatorJob
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Storage.Queries.Timetable as QTimetable
import Tools.Maps as Maps

withoutDuplicateExecution ::
  (MonadMask m, Redis.HedisFlow m r) =>
  Text ->
  Redis.ExpirationTime ->
  m ExecutionResult ->
  m ExecutionResult
withoutDuplicateExecution key timeout action =
  bracket
    (Redis.tryLockRedis key timeout)
    (bool (Redis.unlockRedis key) (pure ()))
    (bool action (pure DuplicateExecution))

data NotImplemented = NotImplemented
  deriving (Show, Exception, IsBaseError)

data BookingNotFound = BookingNotFound
  deriving (Show, Exception, IsBaseError)

handle ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadCatch m
  ) =>
  Job 'AllocateDriverForUpcomingRide ->
  m ExecutionResult
handle (Job {jobData}) =
  Exception.handle (\(SomeException e) -> pure $ Terminate (show e)) $
    withoutDuplicateExecution lockKey (fromIntegral timeout) $ do
      upcomingBooking <- fromMaybeM BookingNotFound =<< QTimetable.findUpcomingBooking jobData.timetableId
      searchRequest <- createSearchForUpcomingBooking upcomingBooking
      quote <- createQuoteForSearchRequest (Id upcomingBooking.providerId) upcomingBooking.farePolicy searchRequest
      booking <- createBookingForQuote quote
      updateTimetableWithBookingId upcomingBooking.id booking.id
      pure Complete
  where
    timeout =
      60

    lockKey =
      "lock:AllocateDriverForUpcomingRide:" <> getId jobData.timetableId

createSearchForUpcomingBooking ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m
  ) =>
  Timetable.UpcomingBooking ->
  m DSearchReq.SearchRequest
createSearchForUpcomingBooking upcomingBooking = do
  let merchantId = Id upcomingBooking.providerId
  (distance, duration) <- do
    res <-
      Maps.getDistance merchantId $
        Maps.GetDistanceReq
          { origin = upcomingBooking.fromLocation,
            destination = upcomingBooking.toLocation,
            travelMode = Just Maps.CAR
          }
    pure (res.distance, res.duration)
  timeZone <- liftIO getCurrentTimeZone
  let pickupTime = localTimeToUTC timeZone upcomingBooking.pickupTime
  now <- getCurrentTime
  id_ <- generateGUID
  fromLocation <- searchRequestLocationFromBookingLocation now upcomingBooking.fromLocation
  toLocation <- searchRequestLocationFromBookingLocation now upcomingBooking.toLocation
  let validTill_ = 60 `addUTCTime` now
      sReq =
        DSearchReq.SearchRequest
          { id = id_,
            transactionId = getId upcomingBooking.recurringBookingId,
            messageId = getId upcomingBooking.id,
            startTime = pickupTime,
            validTill = validTill_,
            providerId = merchantId,
            fromLocation = fromLocation,
            toLocation = toLocation,
            bapId = upcomingBooking.bapId,
            bapUri = upcomingBooking.bapUri,
            estimatedDistance = distance,
            estimatedDuration = duration,
            createdAt = now,
            vehicleVariant = upcomingBooking.farePolicy.vehicleVariant,
            status = DSearchReq.ACTIVE,
            updatedAt = now,
            autoAssignEnabled = True
          }
  Esq.runTransaction $ QSearchReq.create sReq
  pure sReq

searchRequestLocationFromBookingLocation ::
  MonadGuid m =>
  UTCTime ->
  Booking.BookingLocation ->
  m DSearchReq.SearchReqLocation
searchRequestLocationFromBookingLocation now location = do
  id <- generateGUID
  pure $
    DSearchReq.SearchReqLocation
      { id = id,
        lat = location.lat,
        lon = location.lon,
        street = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        building = Nothing,
        areaCode = Nothing,
        area = Nothing,
        full_address = Nothing,
        createdAt = now,
        updatedAt = now
      }

createQuoteForSearchRequest ::
  (EsqDBFlow m r, MonadThrow m) =>
  Id DMerchant.Merchant ->
  FarePolicy.FarePolicy ->
  DSearchReq.SearchRequest ->
  m DDriverQuote.DriverQuote
createQuoteForSearchRequest merchantId farePolicy searchRequest = do
  let distance = searchRequest.estimatedDistance
  fareParams <- calculateFare merchantId farePolicy distance searchRequest.startTime Nothing
  let driverExtraFare = farePolicy.driverExtraFee
  Esq.runTransaction $
    QAllocatorJob.createAllocatorSendSearchRequestToDriverJob 0 $
      SendSearchRequestToDriverJobData
        { requestId = searchRequest.id,
          baseFare = fareParams.baseFare,
          estimatedRideDistance = distance,
          driverMinExtraFee = driverExtraFare.minFee,
          driverMaxExtraFee = driverExtraFare.maxFee
        }
  waitForQuoteSomehow
  where
    waitForQuoteSomehow =
      throw NotImplemented

createBookingForQuote :: MonadThrow m => DDriverQuote.DriverQuote -> m Booking.Booking
createBookingForQuote quote =
  throw NotImplemented

updateTimetableWithBookingId :: MonadThrow m => Id Timetable.Timetable -> Id Booking.Booking -> m ()
updateTimetableWithBookingId timetableId bookingId =
  throw NotImplemented
