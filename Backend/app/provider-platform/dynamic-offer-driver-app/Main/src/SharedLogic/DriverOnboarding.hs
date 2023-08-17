{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverOnboarding where

import qualified Data.Text as T
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Merchant as DTM
import Domain.Types.Person
import Environment
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Tools.Ticket as TT

notifyErrorToSupport ::
  Person ->
  Id DTM.Merchant ->
  Maybe T.Text ->
  T.Text ->
  [Maybe DriverOnboardingError] ->
  Flow ()
notifyErrorToSupport person merchantId driverPhone _ errs = do
  let reasons = catMaybes $ catMaybes $ toMsg <$> errs
  let description = T.intercalate ", " reasons
  _ <- TT.createTicket merchantId (mkTicket description)
  return ()
  where
    toMsg e = toMessage <$> e

    mkTicket description =
      Ticket.CreateTicketReq
        { category = "GENERAL",
          subCategory = Just "DRIVER ONBOARDING ISSUE",
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Nothing,
          name = Just $ person.firstName <> " " <> fromMaybe "" person.lastName,
          phoneNo = driverPhone,
          personId = person.id.getId,
          classification = Ticket.DRIVER,
          rideDescription = Nothing
        }

throwImageError :: Id Domain.Image -> DriverOnboardingError -> Flow b
throwImageError id_ err = do
  _ <- Query.addFailureReason id_ err
  throwError err
