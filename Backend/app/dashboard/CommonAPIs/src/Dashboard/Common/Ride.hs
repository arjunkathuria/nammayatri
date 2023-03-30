{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Dashboard.Common.Ride
  ( module Dashboard.Common.Ride,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data RideEndpoint
  = RideStartEndpoint
  | RideEndEndpoint
  | MultipleRideEndEndpoint
  | RideCancelEndpoint
  | MultipleRideCancelEndpoint
  | RideSyncEndpoint
  | MultipleRideSyncEndpoint
  deriving (Show, Read)

derivePersistField "RideEndpoint"
