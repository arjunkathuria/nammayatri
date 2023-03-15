{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverReferral where

import qualified Domain.Types.DriverReferral as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverReferralT sql=driver_referral
      referralCode Text
      driverId PersonTId
      referredCustomerCount Int
      activatedCustomerCount Int
      linkedAt UTCTime
      Primary referralCode
      Unique DriverReferralDriverId
      deriving Generic
    |]

instance TEntityKey DriverReferralT where
  type DomainKey DriverReferralT = Id Domain.DriverReferral
  fromKey (DriverReferralTKey _id) = Id _id
  toKey (Id id) = DriverReferralTKey id

instance TType DriverReferralT Domain.DriverReferral where
  fromTType DriverReferralT {..} = do
    return $
      Domain.DriverReferral
        { referralCode = Id referralCode,
          driverId = fromKey driverId,
          ..
        }
  toTType Domain.DriverReferral {..} =
    DriverReferralT
      { referralCode = referralCode.getId,
        driverId = toKey driverId,
        ..
      }
