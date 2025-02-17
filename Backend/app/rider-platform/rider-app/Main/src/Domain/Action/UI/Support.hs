{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Support
  ( SendIssueReq (..),
    SafetyCheckSupportReq (..),
    SendIssueRes,
    sendIssue,
    callbackRequest,
    safetyCheckSupport,
  )
where

import Data.OpenApi (ToSchema)
import Domain.Types.Booking (Booking)
import qualified Domain.Types.CallbackRequest as DCallback
import qualified Domain.Types.Issue as DIssue
import Domain.Types.Person as Person
import Domain.Types.Quote (Quote)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import qualified IssueManagement.Common as Domain
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.CallbackRequest as QCallback
import qualified Storage.Queries.Issues as Queries
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Ticket

data Issue = Issue
  { reason :: Text,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SafetyCheckSupportReq = SafetyCheckSupportReq
  { bookingId :: Id Booking,
    isSafe :: Bool,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

validateIssue :: Validate Issue
validateIssue Issue {..} =
  sequenceA_
    [ validateField "reason" reason $ LengthInRange 2 500 `And` text,
      validateField "description" description $ LengthInRange 2 1000 `And` text
    ]
  where
    text = star $ alphanum \/ " " \/ "," \/ "_"

data SendIssueReq = SendIssueReq
  { contactEmail :: Maybe Text,
    issue :: Issue,
    rideBookingId :: Maybe (Id Quote),
    nightSafety :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

validateSendIssueReq :: Validate SendIssueReq
validateSendIssueReq SendIssueReq {..} =
  validateObject "issue" issue validateIssue

type SendIssueRes = APISuccess

sendIssue :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Person.Person -> SendIssueReq -> m SendIssueRes
sendIssue personId request = do
  runRequestValidation validateSendIssueReq request
  newIssue <- buildDBIssue personId request
  Queries.insertIssue newIssue
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  phoneNumber <- mapM decrypt person.mobileNumber
  ticketResponse <- try @_ @SomeException (createTicket person.merchantId person.merchantOperatingCityId (mkTicket newIssue person phoneNumber))
  case ticketResponse of
    Right ticketResponse' -> do
      Queries.updateTicketId newIssue.id ticketResponse'.ticketId
    Left err -> do
      logTagInfo "Create Ticket API failed - " $ show err
  return Success

safetyCheckSupport :: (EncFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Id Person.Person -> SafetyCheckSupportReq -> m SendIssueRes
safetyCheckSupport personId req = do
  ride <- runInReplica $ QRide.findActiveByRBId req.bookingId >>= fromMaybeM (RideDoesNotExist "")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  phoneNumber <- mapM decrypt person.mobileNumber
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  void $ QRide.updateSafetyCheckStatus ride.id $ Just req.isSafe
  when (not req.isSafe && riderConfig.enableSupportForSafety) $ void $ createTicket person.merchantId person.merchantOperatingCityId $ ticketReq ride person phoneNumber req.description
  return Success
  where
    ticketReq ride person phoneNumber description =
      Ticket.CreateTicketReq
        { category = "Code Red",
          subCategory = Just "Night safety check",
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Just [show ride.trackingUrl],
          name = Just $ getName person,
          phoneNo = phoneNumber,
          personId = person.id.getId,
          classification = Ticket.CUSTOMER,
          rideDescription = Just $ rideInfo ride person phoneNumber
        }

    rideInfo ride person phoneNumber =
      Ticket.RideInfo
        { rideShortId = show ride.shortId,
          customerName = Just $ getName person,
          customerPhoneNo = phoneNumber,
          driverName = Just ride.driverName,
          driverPhoneNo = Just ride.driverMobileNumber,
          vehicleNo = ride.vehicleNumber,
          status = show ride.status,
          rideCreatedAt = ride.createdAt,
          pickupLocation = castLocationAPIEntity ride.fromLocation,
          dropLocation = castLocationAPIEntity <$> ride.toLocation,
          fare = Nothing
        }

    castLocationAPIEntity ent =
      Ticket.Location
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

    getName person = fromMaybe "" person.firstName <> " " <> fromMaybe "" person.lastName

buildDBIssue :: MonadFlow m => Id Person.Person -> SendIssueReq -> m DIssue.Issue
buildDBIssue (Id customerId) SendIssueReq {..} = do
  issueId <- L.generateGUID
  time <- getCurrentTime
  return $
    DIssue.Issue
      { id = Id issueId,
        customerId = Id customerId,
        bookingId = rideBookingId,
        contactEmail = contactEmail,
        reason = issue.reason,
        description = issue.description,
        ticketId = Nothing, -- third party id
        status = Domain.OPEN,
        nightSafety = fromMaybe False nightSafety,
        createdAt = time,
        updatedAt = time
      }

mkTicket :: DIssue.Issue -> Person.Person -> Maybe Text -> Ticket.CreateTicketReq
mkTicket issue person phoneNumber =
  Ticket.CreateTicketReq
    { category = "Driver Related",
      subCategory = Just issue.reason,
      issueId = Just issue.id.getId,
      issueDescription = issue.description,
      mediaFiles = Nothing,
      name = Just (fromMaybe "" person.firstName <> " " <> fromMaybe "" person.lastName),
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Nothing
    }

callbackRequest :: (CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> m APISuccess
callbackRequest personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  newCallbackRequest <- buildCallbackRequest person
  _ <- QCallback.create newCallbackRequest
  return Success

buildCallbackRequest :: MonadFlow m => Person.Person -> m DCallback.CallbackRequest
buildCallbackRequest person = do
  guid <- L.generateGUID
  now <- getCurrentTime
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  customerMobileCountryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  return $
    DCallback.CallbackRequest
      { id = Id guid,
        merchantId = person.merchantId,
        customerName = person.firstName,
        customerPhone,
        customerMobileCountryCode,
        status = DCallback.PENDING,
        createdAt = now,
        updatedAt = now
      }
