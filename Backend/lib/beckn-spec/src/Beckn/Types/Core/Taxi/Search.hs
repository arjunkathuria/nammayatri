{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search
  ( module Beckn.Types.Core.Taxi.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Search.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Beckn.Types.Core.Taxi.Search.Time as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.External.Maps (RouteInfo)

data SearchMessage = SearchMessage
  { intent :: Intent,
    routeInfo :: Maybe RouteInfo
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON SearchMessage where
  toJSON = genericToJSON searchMessageJSONOptions

instance FromJSON SearchMessage where
  parseJSON = genericParseJSON searchMessageJSONOptions

searchMessageJSONOptions :: Options
searchMessageJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "routeInfo" -> "./nammayatri/routeInfo"
        a -> a
    }
