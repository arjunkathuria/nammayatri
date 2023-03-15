{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Ride where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "ride"
    :> ShareRideInfoAPI

type ShareRideInfoAPI = Common.ShareRideInfoAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = shareRideInfo

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId reqRideId = withFlowHandlerAPI $ DRide.shareRideInfo merchantShortId reqRideId
