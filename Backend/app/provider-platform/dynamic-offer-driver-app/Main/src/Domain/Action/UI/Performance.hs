module Domain.Action.UI.Performance where

import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (throwError)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import Storage.Queries.DriverReferral as SQD

data Results = Results
  { totalReferredCustomers :: Int,
    totalActivatedCustomers :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype PerformanceRes = PerformanceRes
  { referrals :: Results
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDriverPerformance :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SP.Person -> m PerformanceRes
getDriverPerformance driverId = do
  driverReferral <- SQD.findById driverId
  case driverReferral of
    Just driverReferral' -> pure $ PerformanceRes (Results (driverReferral'.referredCustomerCount) (driverReferral'.activatedCustomerCount))
    Nothing -> throwError $ InvalidRequest "Driver doesn't has referralCode"
