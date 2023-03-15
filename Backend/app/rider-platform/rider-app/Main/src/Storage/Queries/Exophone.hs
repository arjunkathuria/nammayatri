{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Exophone where

import Domain.Types.Exophone
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Exophone

-- TODO add cached queries
findByPrimaryPhone :: Transactionable m => Text -> m (Maybe Exophone)
findByPrimaryPhone primaryPhone = do
  findOne $ do
    exophone <- from $ table @ExophoneT
    where_ $ exophone ^. ExophonePrimaryPhone ==. val primaryPhone
    return exophone

findAllByMerchantId :: Transactionable m => Id DM.Merchant -> m [Exophone]
findAllByMerchantId merchantId = do
  findAll $ do
    exophone <- from $ table @ExophoneT
    where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)
    return exophone
