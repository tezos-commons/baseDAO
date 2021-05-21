-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ligo.BaseDAO.Common
  ( DaoOriginateData(..)
  , OriginateFn
  , totalSupplyFromLedger

  , frozenTokens
  , unfrozenTokens
  , unfrozenTokens1
  , unknownTokens

  , mkFA2View
  , checkTokenBalance
  , dummyFA2Contract
  , makeProposalKey
  , addDataToSign
  , permitProtect
  , sendXtz

  , createSampleProposal
  , createSampleProposals
  , originateLigoDaoWithBalance
  , originateLigoDaoWithConfigDesc
  , originateLigoDao

  -- * Re-export
  , module StorageHelper
  ) where

import Universum hiding (drop, swap)

import Lorentz hiding (now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test (contractConsumer)
import Michelson.Typed.Convert (convertContract, untypeValue)
import Morley.Nettest
import Morley.Nettest.Caps (MonadOps)
import Named ((!))
import Util.Named

import qualified Data.Map as M
import qualified Data.Set as S
import Ligo.BaseDAO.Contract
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common.StorageHelper as StorageHelper
import Test.Ligo.BaseDAO.Proposal.Config (ConfigDesc, fillConfig)

type OriginateFn m = m DaoOriginateData

data DaoOriginateData = DaoOriginateData
  { dodOwner1 :: Address
  , dodOperator1 :: Address
  , dodOwner2 :: Address
  , dodOperator2 :: Address
  , dodDao :: TAddress Parameter
  , dodTokenContract :: TAddress FA2.Parameter
  , dodAdmin :: Address
  , dodGuardian :: TAddress (Address, ProposalKey)
  , dodPeriod :: Natural
  }

-- | A dummy contract with FA2 parameter that remembers the
-- transfer calls.
dummyFA2Contract :: Contract FA2.Parameter [FA2.TransferParams]
dummyFA2Contract = defaultContract $
  unpair #
    (entryCase @FA2.Parameter (Proxy @PlainEntrypointsKind)
      ( #cBalance_of /-> Lorentz.drop
      , #cTransfer /-> cons
      , #cUpdate_operators /-> Lorentz.drop
      )) # nil # pair

-- | A dummy contract that act as guardian contract for BaseDAO
dummyGuardianContract :: Contract (Address, ProposalKey) ()
dummyGuardianContract = defaultContract $
  unpair # unpair #
  stackType @'[Address, ProposalKey, ()] #
  contractCalling @Parameter (Call @"Drop_proposal") #
  ifSome (
      swap #
      push zeroMutez # swap #
      transferTokens # nil # swap # cons
    ) (drop # nil) #
  stackType @'[[Operation], ()] #
  pair

totalSupplyFromLedger :: Ledger -> TotalSupply
totalSupplyFromLedger (BigMap ledger) =
  M.foldrWithKey (\(_, tokenId) val totalSup ->
      M.alter (\v -> Just (fromMaybe 0 v + val)) tokenId totalSup
    )
    (M.fromList [(frozenTokenId, 0)])
    ledger

frozenTokens :: FA2.TokenId
frozenTokens = frozenTokenId

unfrozenTokens :: FA2.TokenId
unfrozenTokens = FA2.TokenId 42

unfrozenTokens1 :: FA2.TokenId
unfrozenTokens1 = FA2.TokenId 420

unknownTokens :: FA2.TokenId
unknownTokens = FA2.TokenId 2

-- | Create FA2 View
mkFA2View
  :: forall paramFa a r contract. ToContractRef r contract
  => a
  -> contract
  -> FA2.FA2View paramFa a r
mkFA2View a c = FA2.FA2View (mkView a c)

-- | Helper function to check a user balance of a particular token
checkTokenBalance
  :: (MonadNettest caps base m, HasCallStack)
  => FA2.TokenId -> TAddress Parameter
  -> Address -> Natural
  -> m ()
checkTokenBalance tokenId dodDao addr expectedValue = withFrozenCallStack $ do
  consumer <- originateSimple "consumer" [] contractConsumer

  withSender addr $ call dodDao (Call @"Balance_of")
    (mkFA2View [ FA2.BalanceRequestItem
      { briOwner = addr
      , briTokenId = tokenId
      } ] consumer)

  checkStorage (toAddress consumer)
    (toVal [[((addr, tokenId), expectedValue)]] )

makeProposalKey :: ProposeParams -> Address -> ProposalKey
makeProposalKey params owner = toHashHs $ lPackValue (params, owner)

addDataToSign
  :: (MonadNettest caps base m)
  => TAddress param
  -> Nonce
  -> d
  -> m (DataToSign d, d)
addDataToSign (toAddress -> dsContract) dsNonce dsData = do
  dsChainId <- getChainId
  return (DataToSign{..}, dsData)

-- | Add a permit from given user.
permitProtect
  :: (MonadNettest caps base m, NicePackedValue a)
  => Address -> (DataToSign a, a) -> m (PermitProtected a)
permitProtect author (toSign, a) = do
  pKey <- getPublicKey author
  pSignature <- signBinary (lPackValue toSign) author
  return PermitProtected
    { ppArgument = a
    , ppPermit = Just Permit{..}
    }

sendXtz
  :: (MonadNettest caps base m, HasCallStack, NiceParameter pm)
  => Address -> EpName -> pm -> m ()
sendXtz addr epName pm = withFrozenCallStack $ do
  let transferData = TransferData
        { tdTo = addr
        , tdAmount = toMutez 0.5_e6 -- 0.5 xtz
        , tdEntrypoint = epName
        , tdParameter = pm
        }
  transfer transferData


-- TODO: Implement this via [#31] instead
-- checkIfAProposalExist
--   :: MonadNettest caps base m
--   => ProposalKey -> TAddress (Parameter TestProposalMetadata) -> m ()
-- checkIfAProposalExist proposalKey dodDao = do
--   owner :: Address <- newAddress "owner"
--   consumer <- originateSimple "consumer" [] contractConsumer
--   -- | If the proposal exists, there should be no error
--   callFrom owner dodDao (Call @"Proposal_metadata") (mkView proposalKey consumer)

-- TODO [#31]: See this ISSUES: https://gitlab.com/morley-framework/morley/-/issues/415#note_435327096
-- Check if certain field in storage
-- checkPropertyOfProposal :: _
-- checkPropertyOfProposal = error "undefined"

originateLigoDaoWithBalance
 :: MonadNettest caps base m
 => ContractExtra
 -> Config
 -> (Address -> Address -> [(LedgerKey, LedgerValue)])
 -> OriginateFn m
originateLigoDaoWithBalance extra config balFunc = do
  owner1 :: Address <- newAddress "owner1"
  operator1 :: Address <- newAddress "operator1"
  owner2 :: Address <- newAddress "owner2"
  operator2 :: Address <- newAddress "operator2"

  admin :: Address <- newAddress "admin"

  let bal = BigMap $ M.fromList $ balFunc owner1 owner2
  let operators = BigMap $ M.fromSet (const ()) $ S.fromList
        [ (#owner .! owner1, #operator .! operator1, #token_id .! unfrozenTokens)
        , (#owner .! owner2, #operator .! operator2, #token_id .! unfrozenTokens)
        ]

  currentLevel <- getLevel
  tokenContract <- originateSimple "TokenContract" [] dummyFA2Contract
  guardianContract <- originateSimple "guardian" () dummyGuardianContract

  let fullStorage = FullStorage'
        { fsStorage =
            ( mkStorage
              ! #extra extra
              ! #admin admin
              ! #metadata mempty
              ! #tokenAddress (unTAddress tokenContract)
              ! #level currentLevel
              ! #quorumThreshold (fromIntegral $ cMinQuorumThreshold config)
            )
            { sLedger = bal
            , sOperators = operators
            , sTotalSupply = totalSupplyFromLedger bal
            , sGuardian = unTAddress guardianContract
            }
        , fsConfig = config
        }
  let
    originateData = UntypedOriginateData
      { uodName = "BaseDAO"
      , uodBalance = toMutez 0
      , uodStorage = untypeValue $ toVal $ fullStorage
      , uodContract = convertContract baseDAOContractLigo
      }

  daoUntyped <- originateUntyped originateData

  let dao = TAddress @Parameter daoUntyped

  pure $ DaoOriginateData owner1 operator1 owner2 operator2 dao tokenContract
      admin guardianContract (unPeriod $ cPeriod config)

originateLigoDaoWithConfig
 :: MonadNettest caps base m
 => ContractExtra
 -> Config
 -> OriginateFn m
originateLigoDaoWithConfig extra config =
  originateLigoDaoWithBalance extra config
    (\owner1_ owner2_ ->
    [ ((owner1_, frozenTokenId), 100)
    , ((owner2_, frozenTokenId), 100)
    ])

originateLigoDaoWithConfigDesc
 :: MonadNettest caps base m
 => ContractExtra
 -> ConfigDesc Config
 -> OriginateFn m
originateLigoDaoWithConfigDesc extra config =
  originateLigoDaoWithBalance extra (fillConfig config defaultConfig)
    (\owner1_ owner2_ ->
    [ ((owner1_, frozenTokenId), 100)
    , ((owner2_, frozenTokenId), 100)
    ])

originateLigoDao :: MonadNettest caps base m => OriginateFn m
originateLigoDao =
  originateLigoDaoWithConfig dynRecUnsafe defaultConfig

createSampleProposal
  :: (MonadNettest caps base m, HasCallStack)
  => Int -> Address -> TAddress Parameter -> m ProposalKey
createSampleProposal counter dodOwner dao = do
  let (pk, action) = createSampleProposal_ counter dodOwner dao
  withSender dodOwner action
  pure pk

createSampleProposal_
  :: (MonadOps m, HasCallStack)
  => Int -> Address -> TAddress Parameter -> (ProposalKey, m ())
createSampleProposal_ counter dodOwner1 dao =
  let params = ProposeParams
        { ppFrozenToken = 10
        , ppProposalMetadata = lPackValueRaw @Integer $ fromIntegral counter
        }
  in (makeProposalKey params dodOwner1, call dao (Call @"Propose") params)

-- TODO consider making this polymorphic on the input/output size
createSampleProposals
  :: (MonadNettest caps base m, HasCallStack)
  => (Int, Int) -> Address -> TAddress Parameter -> m (ProposalKey, ProposalKey)
createSampleProposals (counter1, counter2) dodOwner1 dao = do
  let (pk1, action1) = createSampleProposal_ counter1 dodOwner1 dao
  let (pk2, action2) = createSampleProposal_ counter2 dodOwner1 dao
  withSender dodOwner1 . inBatch $ do
    action1
    action2
    return ()
  pure (pk1, pk2)
