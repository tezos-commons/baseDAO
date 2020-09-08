-- SPDX-FileCopyrightText: 2020 Tocqueville Group
--
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Lorentz.Contracts.ManagedLedger.Types
  ( Storage
  , StorageC
  , StorageSkeleton (..)
  , LedgerC
  , LedgerValue
  , mkStorage
  , mkStorageSkeleton
  , storageSkeletonNotes
  , storageNotes
  ) where

import Prelude (sum, (<$>))
import Util.Named ((.!))

-- import Indigo as I (HasField(..), fieldLensADT, fieldLensDeeper)
import Lorentz
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import Michelson.Typed (Notes(..))
import Michelson.Untyped (ann, noAnn)

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

type LedgerValue = "balance" :! Natural

data StorageSkeleton fields = StorageSkeleton
  { ledger    :: BigMap Address LedgerValue
  , approvals :: BigMap GetAllowanceParams Natural
  , fields    :: fields
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance (StoreHasField fields fname ftype, IsoValue fields) =>
    StoreHasField (StorageSkeleton fields) fname ftype where
  storeFieldOps = storeFieldOpsDeeper #fields

instance IsoValue fields =>
    StoreHasSubmap (StorageSkeleton fields) "ledger" Address LedgerValue where
  storeSubmapOps = storeSubmapOpsDeeper #ledger

instance IsoValue fields =>
    StoreHasSubmap (StorageSkeleton fields) "approvals" GetAllowanceParams Natural where
  storeSubmapOps = storeSubmapOpsDeeper #approvals

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorageSkeleton :: Map Address Natural -> fields -> StorageSkeleton fields
mkStorageSkeleton balances flds = StorageSkeleton
  { ledger    = BigMap $ toLedgerValue <$> balances
  , approvals = mempty
  , fields    = flds
  }
  where
    toLedgerValue initBal = #balance .! initBal

-- | A temporary approach to add field annotations to 'StorageSkeleton'.
storageSkeletonNotes :: Notes (ToT fields) -> Notes (ToT (StorageSkeleton fields))
storageSkeletonNotes fieldNotes = NTPair noAnn (ann "ledger") noAnn
  (NTBigMap noAnn (NTAddress $ ann "user") (NTNat $ ann "balance"))
  (NTPair noAnn (ann "approvals") noAnn
    (NTBigMap noAnn
      (NTPair noAnn noAnn noAnn
        (NTAddress $ ann "owner")
        (NTAddress $ ann "spender")
      )
      (NTNat $ ann "value")
    )
    fieldNotes
  )

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc StorageFields where
  typeDocMdDescription = "Managed ledger storage fields."

  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance HasFieldOfType StorageFields name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

type Storage = StorageSkeleton StorageFields

instance TypeHasDoc a => TypeHasDoc (StorageSkeleton a) where
  typeDocMdDescription = "Managed ledger storage skeleton."

  typeDocMdReference = poly1TypeDocMdReference
  typeDocHaskellRep = concreteTypeDocHaskellRep @(StorageSkeleton StorageFields)
  typeDocMichelsonRep = concreteTypeDocMichelsonRep @(StorageSkeleton StorageFields)

-- ----------------------------------------------------------------------------
-- -- HasField instances for Indigo version
-- ----------------------------------------------------------------------------

-- instance I.HasField Storage "ledger" (BigMap Address LedgerValue) where
--   fieldLens = fieldLensADT #ledger

-- instance I.HasField Storage "approvals" (BigMap GetAllowanceParams Natural) where
--   fieldLens = fieldLensADT #approvals

-- instance I.HasField StorageFields "admin" Address where
--   fieldLens = fieldLensADT #admin

-- instance I.HasField StorageFields "paused" Bool where
--   fieldLens = fieldLensADT #paused

-- instance I.HasField StorageFields "totalSupply" Natural where
--   fieldLens = fieldLensADT #totalSupply

-- instance I.HasField Storage "admin" Address where
--   fieldLens = fieldLensDeeper #fields

-- instance I.HasField Storage "paused" Bool where
--   fieldLens = fieldLensDeeper #fields

-- instance I.HasField Storage "totalSupply" Natural where
--   fieldLens = fieldLensDeeper #fields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorageSkeleton balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  }

type StorageC store =
  ( LedgerC store
  , StorageContains store
   [ "admin" := Address
   , "paused" := Bool
   ]
  )

type LedgerC store = StorageContains store
  [ "totalSupply" := Natural
  , "ledger" := Address ~> LedgerValue
  , "approvals" := GetAllowanceParams ~> Natural
  ]

-- | A temporary approach to add field annotations to 'Storage'.
storageNotes :: Notes (ToT Storage)
storageNotes = storageSkeletonNotes @StorageFields $
  (NTPair noAnn noAnn noAnn
    (NTAddress $ ann "admin")
    (NTPair noAnn noAnn noAnn (NTBool $ ann "paused") (NTNat $ ann "totalSupply"))
  )
