{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant
  ( findById,
    findByShortId,
    findBySubscriberId,
    update1,
    update2,
    loadAllProviders,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Class as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe Merchant)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
    Nothing -> flip whenJust cacheMerchant /=<< Queries.findById id

findBySubscriberId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeSubscriberIdKey subscriberId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findBySubscriberId subscriberId

findByShortId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeShortIdKey shortId) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(MerchantD 'Unsafe) @Merchant a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheMerchant /=<< Queries.findByShortId shortId

-- Call it after any update
clearCache :: HedisFlow m r => Merchant -> m ()
clearCache org = do
  Hedis.withCrossAppRedis $ do
    Hedis.del (makeIdKey org.id)
    Hedis.del (makeShortIdKey org.shortId)
    Hedis.del (makeSubscriberIdKey org.subscriberId)

cacheMerchant :: (HasCacheConfig r, HedisFlow m r) => Merchant -> m ()
cacheMerchant org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey org.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @Merchant @(MerchantD 'Unsafe) org) expTime
    Hedis.setExp (makeShortIdKey org.shortId) idKey expTime
    Hedis.setExp (makeSubscriberIdKey org.subscriberId) idKey expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "driver-offer:CachedQueries:Merchant:Id-" <> id.getId

makeSubscriberIdKey :: ShortId Subscriber -> Text
makeSubscriberIdKey subscriberId = "driver-offer:CachedQueries:Merchant:SubscriberId-" <> subscriberId.getShortId

makeShortIdKey :: ShortId Merchant -> Text
makeShortIdKey shortId = "driver-offer:CachedQueries:Merchant:ShortId-" <> shortId.getShortId

-- TODO fix other queries
-- solution 1
update1 :: forall m r. (HedisFlow m r, Esq.Finalize m) => Proxy m -> Merchant -> Esq.SqlDB ()
update1 _ merchant = do
  Queries.update merchant
  Esq.finalize @m $ clearCache merchant

-- solution 2
update2 :: forall m r. (HedisFlow m r, Esq.Finalize m) => (m () -> Esq.SqlDB ()) -> Merchant -> Esq.SqlDB ()
update2 finalize merchant = do
  Queries.update merchant
  finalize $ clearCache merchant

loadAllProviders :: Esq.Transactionable m => m [Merchant]
loadAllProviders = Queries.loadAllProviders
