{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Track
  ( TrackBuildReq (..),
    buildTrackReq,
  )
where

import qualified Beckn.Types.Core.Taxi.Track as Track
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common

data TrackBuildReq = TrackBuildReq
  { bppRideId :: Id DRide.BPPRide,
    bppId :: Text,
    bppUrl :: BaseUrl,
    transactionId :: Text
  }

buildTrackReq ::
  ( MonadFlow m,
    HasBapInfo r m,
    HedisFlow m r
  ) =>
  TrackBuildReq ->
  m (BecknReq Track.TrackMessage)
buildTrackReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  Redis.setExp (key messageId) res.bppRideId 1800 --30 mins
  context <- buildTaxiContext Context.TRACK messageId (Just res.transactionId) bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl)
  pure $ BecknReq context $ mkTrackMessage res
  where
    key messageId = "Track:bppRideId:" <> messageId

mkTrackMessage :: TrackBuildReq -> Track.TrackMessage
mkTrackMessage res = Track.TrackMessage $ Track.Order res.bppRideId.getId
