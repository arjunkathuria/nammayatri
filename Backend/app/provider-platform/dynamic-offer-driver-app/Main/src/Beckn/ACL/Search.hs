{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildSearchReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Search.SearchReq ->
  m DSearch.DSearchReq
buildSearchReq subscriber req = do
  let context = req.context
  validateContext Context.SEARCH context
  let intent = req.message.intent
      pickup = intent.fulfillment.start
      pickupTime =
        case pickup.time.days of
          Just days ->
            DSearch.Recurring pickup.time.timestamp days
          Nothing ->
            DSearch.OneTime pickup.time.timestamp
      dropOff = fromJust intent.fulfillment.end
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  pure
    DSearch.DSearchReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation = mkLocation pickup.location,
        pickup = pickupTime,
        dropLocation = mkLocation dropOff.location,
        routeInfo = req.message.routeInfo
      }

mkLocation :: Search.Location -> Location.SearchReqLocationAPIEntity
mkLocation (Search.Location Search.Gps {..} _) =
  Location.SearchReqLocationAPIEntity
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
