-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Common
  ( mkFA2View
  , checkTokenBalance
  , withOriginated

  , TestSetup (..)
  , TestOwnerSetup (..)
  , (:/)(..)

  , originateTrivialDao
  , originateBaseDaoWithConfig
  , originateTrivialDaoWithBalance
  , makeProposalKey
  ) where

import Universum

import qualified Data.Map as M
import qualified Data.Set as S
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Morley.Nettest
import Named (defaults, (!))
import Tezos.Crypto (blake2b)
import Util.Named ((.!))

import qualified Lorentz.Contracts.BaseDAO as DAO
import Lorentz.Contracts.BaseDAO.Types
import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.TrivialDAO as DAO

-- | Function that originates the contract and also make a bunch of
-- address (the `addrCount` arg determines the count) for use within
-- the tests. It is not pretty, but IMO it makes the test a bit less
-- verbose.
withOriginated
  :: MonadNettest caps base m
  => Integer
  -> ([Address] -> (Storage () ()))
  -> ([Address] -> TAddress (Parameter ()) -> m a)
  -> m a
withOriginated addrCount storageFn tests = do
  addresses <- mapM (\x -> newAddress $ "address" <> (show x)) [1 ..addrCount]
  baseDao <- originate $ OriginateData
    { odFrom = nettestAddress
    , odName = "BaseDAO Test Contract"
    , odBalance = zeroMutez
    , odStorage = storageFn addresses
    , odContract = DAO.baseDaoContract DAO.defaultConfig
    }
  tests addresses baseDao

-- | Keeps all the parameters of the current test.
data TestSetup pm = TestSetup
  { tsDao :: TAddress (Parameter pm)
  , tsAdmin :: Address
  , tsOwners :: (TestOwnerSetup, TestOwnerSetup)
  }

data TestOwnerSetup = TestOwnerSetup
  { tosAddress :: Address
    -- ^ Owner address
  , tosOperator :: Address
    -- ^ Related operator address
  }

{- Note about producing test setup:

There seems to be no way to work with TestSetup datatype directly in a
non-verbose way, so further we try to make is possible to pick only the
necessary parts of test setup with ease.
-}

-- | Injection from @a@ to @b@.
infix 0 >->
class a >-> b where
  inj :: a -> b

instance a >-> () where
  inj _ = ()

-- | Product of two types.
infixr 1 :/
data a :/ b = a :/ b

instance (a >-> b, a >-> c) => a >-> b :/ c where
  inj a = inj a :/ inj a

-- | Extract test setup itself.
instance (pm ~ pm') => TestSetup pm >-> TestSetup pm' where
  inj = id

-- | Extract DAO address.
instance (cp ~ Parameter pm) => TestSetup pm >-> TAddress cp where
  inj = tsDao

-- | Extract owners.
instance (TestOwnerSetup >-> e1, TestOwnerSetup >-> e2) =>
          TestSetup pm >-> (e1, e2) where
  inj tos = case tsOwners tos of
    (o1, o2) -> (inj o1, inj o2)

instance TestOwnerSetup >-> TestOwnerSetup where
  inj = id

instance TestOwnerSetup >-> Address where
  inj = tosAddress

instance (e1 ~ Address, e2 ~ Address) => TestOwnerSetup >-> (e1, e2) where
  inj = tosAddress &&& tosOperator

-- | Helper functions which originate BaseDAO with a predefined owners, operators and initial storage.
-- used in FA2 Proposals tests.
originateBaseDaoWithConfig
  :: forall pm ce setup caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     , TestSetup pm >-> setup
     )
  => ce -> DAO.Config ce pm -> m (TAddress (Parameter pm) :/ setup)
originateBaseDaoWithConfig contractExtra config = do
  originateBaseDaoWithBalance contractExtra config
    (\owner1 owner2 ->
        [ ((owner1, 0), 100)
        , ((owner2, 0), 100)
        ]
    )

originateBaseDaoWithBalance
  :: forall pm ce setup caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     , TestSetup pm >-> setup
     )
  => ce
  -> DAO.Config ce pm
  -> (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m (TAddress (Parameter pm) :/ setup)
originateBaseDaoWithBalance contractExtra config balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = M.fromList $ balFunc owner1 owner2
  let operators = M.fromSet (const ()) $ S.fromList
        [ (#owner .! owner1, #operator .! operator1)
        , (#owner .! owner2, #operator .! operator2)
        ]

  let
    originateData = OriginateData
      { odFrom = nettestAddress
      , odName = "BaseDAO"
      , odBalance = toMutez 0
      , odStorage = (mkStorage ! #admin admin ! #extra contractExtra ! defaults)
        { sLedger = BigMap bal
        , sOperators = BigMap operators
        }
      , odContract = DAO.baseDaoContract config
      }
  dao <- originate originateData

  pure $ inj TestSetup
    { tsDao = dao
    , tsAdmin = admin
    , tsOwners =
        ( TestOwnerSetup owner1 operator1
        , TestOwnerSetup owner2 operator2
        )
    }

originateTrivialDaoWithBalance
  :: (MonadNettest caps base m, TestSetup () >-> setup)
  => (Address -> Address -> [((Address, FA2.TokenId), Natural)])
  -> m (TAddress (Parameter ()) :/ setup)
originateTrivialDaoWithBalance =
  originateBaseDaoWithBalance () DAO.trivialConfig

originateBaseDao
  :: forall pm ce setup caps base m.
     ( NiceStorage (Storage ce pm), NiceParameterFull (Parameter pm), NiceStorage pm
     , TypeHasDoc pm, NicePackedValue pm
     , KnownValue ce, TypeHasDoc ce
     , MonadNettest caps base m
     , TestSetup pm >-> setup
     )
  => ce -> m (TAddress (Parameter pm) :/ setup)
originateBaseDao contractExtra = originateBaseDaoWithConfig contractExtra DAO.defaultConfig

originateTrivialDao
  :: (MonadNettest caps base m, TestSetup () >-> setup)
  => m (TAddress (Parameter ()) :/ setup)
originateTrivialDao = originateBaseDao ()

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (NiceParameterFull (Parameter pm), MonadNettest caps base m)
  => FA2.TokenId -> TAddress (DAO.Parameter pm)
  -> Address -> Natural
  -> m ()
checkTokenBalance tokenId dao addr expectedValue = do
  consumer <- originateSimple "consumer" [] contractConsumer

  callFrom (AddressResolved addr) dao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (AddressResolved $ toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )

makeProposalKey :: NicePackedValue pm => DAO.ProposeParams pm -> Address -> ByteString
makeProposalKey params owner = blake2b $ lPackValue (params, owner)
