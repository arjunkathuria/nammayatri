{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Init where

import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.ItemId as ItemId
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Kernel.Prelude
import qualified Kernel.Product.Validation.Context as Context
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Error.Throwing

buildInitReq ::
  ( MonadThrow m,
    HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  let context = req.context
  Context.validateContext Context.INIT context
  let order = req.message.order
  item <- case order.items of
    [it] -> pure it
    _ -> throwError $ InvalidRequest "There must be exactly one item in init request"
  itemId <- item.id & fromMaybeM (InvalidRequest "Item id required")

  -- should we check start time and other details?
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")

  pure $
    case ItemId.parseText itemId of
      Just (ItemId.RecurringTrip variant) ->
        let pickup = order.fulfillment.start
            pickupLocation = mkLocation pickup.location
            pickupTime = pickup.time.timestamp
            pickupSchedule = fromMaybe mempty pickup.time.days
            dropoff = fromJust $ order.fulfillment.end
            dropLocation = mkLocation dropoff.location
         in DInit.InitRecurringBookingReq $
              DInit.InitRecurringBooking
                { bapId = subscriber.subscriber_id,
                  bapUri = subscriber.subscriber_url,
                  ..
                }
      _ ->
        DInit.InitOneWayTripReq $
          DInit.InitOneWayTrip
            { driverQuoteId = Id itemId,
              bapId = subscriber.subscriber_id,
              bapUri = subscriber.subscriber_url
            }

mkLocation :: Search.Location -> DLoc.SearchReqLocationAPIEntity
mkLocation (Search.Location Search.Gps {..} _) =
  DLoc.SearchReqLocationAPIEntity
    { areaCode = Nothing,
      street = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      area = Nothing,
      full_address = Nothing,
      ..
    }
