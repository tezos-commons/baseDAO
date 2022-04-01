-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Ligo.BaseDAO.Token
  ( test_BaseDAO_Token
  ) where

import Universum

import Lorentz hiding (assert, (>>))
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Michelson.Runtime.GState (genesisAddress1, genesisAddress2)
import Test.Cleveland
import Test.Tasty (TestTree, testGroup)

import Ligo.BaseDAO.ErrorCodes
import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

test_BaseDAO_Token :: TestTree
test_BaseDAO_Token = testGroup "BaseDAO non-FA2 token tests:"
  [ testScenario "can call transfer tokens entrypoint" $ scenario
      $ transferContractTokensScenario (originateLigoDao @'Base)
  ,  testScenario "can transfer funds to the contract" $ scenario
      $ ensureXtzTransfer (originateLigoDao @'Base)
  ]

transferContractTokensScenario
  :: MonadCleveland caps m
  => OriginateFn 'Base m -> m ()
transferContractTokensScenario originateFn = do
  DaoOriginateData{..} <- originateFn defaultQuorumThreshold
  let target_owner1 = genesisAddress1
  let target_owner2 = genesisAddress2

  let transferParams = [ FA2.TransferItem
            { tiFrom = target_owner1
            , tiTxs = [ FA2.TransferDestination
                { tdTo = target_owner2
                , tdTokenId = FA2.theTokenId
                , tdAmount = 10
                } ]
            } ]
      param = TransferContractTokensParam
        { tcContractAddress = toAddress dodTokenContract
        , tcParams = transferParams
        }

  withSender dodOwner1 $
    call dodDao (Call @"Transfer_contract_tokens") param
    & expectFailedWith notAdmin

  withSender dodAdmin $
    call dodDao (Call @"Transfer_contract_tokens") param

  tcStorage <- getStorage @[[FA2.TransferItem]] (unTAddress dodTokenContract)

  assert (tcStorage ==
    ([ [ FA2.TransferItem { tiFrom = target_owner1, tiTxs = [FA2.TransferDestination { tdTo = target_owner2, tdTokenId = FA2.theTokenId, tdAmount = 10 }] } ]
      ])) "Unexpected FA2 transfers"

ensureXtzTransfer
  :: MonadCleveland caps m
  => OriginateFn a m -> m ()
ensureXtzTransfer originateFn = do
  DaoOriginateData{..} <- originateFn defaultQuorumThreshold
  sendXtz dodDao
