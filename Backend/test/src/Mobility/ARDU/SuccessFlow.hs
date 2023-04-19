{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.SuccessFlow where

import Common
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import HSpec
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Common (HighPrecMeters, Meters)
import Kernel.Utils.Common (addUTCTime)
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import qualified Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl API.getDriverOfferBppBaseUrl
  describe "Successful flow, location updates" $
    beforeAndAfter_
      ( do
          Utils.resetDriver arduDriver1
          Utils.resetCustomer appRegistrationToken
      )
      $ do
        it "Testing success flow and location updates for short curvy route" $
          defaultSuccessFlow 30 680 680 karnatakaLocationUpdates clients
        it "Testing success flow and location updates for the route with far isolated point" $
          defaultSuccessFlow 700 8000 8000 karnatakaRouteIsolatedPoint clients
        it "Testing success flow and location updates with reversed points list" $
          reversedPointsListSuccessFlow 700 8000 8000 karnatakaRouteIsolatedPoint clients
        it "Testing success flow and location updates with outdated points" $
          outdatedPointsSuccessFlow 700 7000 8000 karnatakaRouteIsolatedPoint clients
        it "Testing success flow and location updates called multiple times at the same time " $
          raceConditionSuccessFlow 700 8000 8000 karnatakaRouteIsolatedPoint clients

waitBetweenUpdates :: Int
waitBetweenUpdates = 1e5 + 1e6 * fromIntegral timeBetweenLocationUpdates

defaultSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
defaultSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ API.buildUpdateLocationRequest upd
      void . callBPP $ API.ui.location.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates

reversedPointsListSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
reversedPointsListSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ API.buildUpdateLocationRequest upd
      void . callBPP $ API.ui.location.updateLocation arduDriver1.token $ NE.reverse updReq
      liftIO $ threadDelay waitBetweenUpdates

-- There was a bug, when it was possible to update location multiple times if
-- call updateLocation with outdated points
outdatedPointsSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
outdatedPointsSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      updReq <- liftIO $ makePointsOutdated <$> API.buildUpdateLocationRequest upd
      void . callBPP $ API.ui.location.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    makePointsOutdated = fmap (\point -> point{ts = addUTCTime (negate 600) point.ts})

raceConditionSuccessFlow :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientEnvs -> IO ()
raceConditionSuccessFlow eps distance chargeableDistance updates clients = withBecknClients clients $ do
  successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates $ do
    forM_ (NE.toList updates) $ \upd -> do
      void . forkMultipleThreads 5 $ do
        updReq <- liftIO $ API.buildUpdateLocationRequest upd
        void . callBPP $ API.ui.location.updateLocation arduDriver1.token updReq
      liftIO $ threadDelay waitBetweenUpdates
  where
    forkMultipleThreads a f = replicateM a . liftIO $ forkIO $ withBecknClients clients f

successFlowWithLocationUpdatesHandler :: Double -> HighPrecMeters -> Meters -> LocationUpdates -> ClientsM () -> ClientsM ()
successFlowWithLocationUpdatesHandler eps distance chargeableDistance updates locationUpdatesFunc = do
  let (origin, destination, searchReq') = searchReqFromUpdatesList updates

  Utils.setupDriver arduDriver1 origin
  scRes <- Utils.search'Confirm appRegistrationToken arduDriver1 searchReq'
  let tRide = scRes.ride
      bBookingId = scRes.bapBookingId

  Utils.startRide arduDriver1 origin tRide bBookingId
  ----
  liftIO $ threadDelay waitBetweenUpdates
  locationUpdatesFunc
  liftIO $ threadDelay waitBetweenUpdates
  ----
  Utils.endRide arduDriver1 destination tRide bBookingId

  tRide' <- Utils.getBPPRideById tRide.id
  tRide'.traveledDistance `shouldSatisfy` equalsEps (realToFrac eps) distance
  tRide'.chargeableDistance `shouldSatisfy` (equalsEps (roundToIntegral eps) chargeableDistance . fromJust)

-- Leave feedback
-- not yet implemented
--  void . callBAP $ callAppFeedback 5 completedRideId
