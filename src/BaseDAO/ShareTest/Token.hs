-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module BaseDAO.ShareTest.Token
  ( burnScenario
  , mintScenario
  , transferContractTokensScenario
  ) where

import Universum

import Lorentz hiding ((>>))
import Morley.Nettest
import Util.Named

import qualified Lorentz.Contracts.BaseDAO.Types as DAO
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import BaseDAO.ShareTest.Common

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}


burnScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, DAO.ParameterC param pm)
  => OriginateFn param m -> m ()
burnScenario originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn

  withSender (AddressResolved owner1) $ call dao (Call @"Burn") (DAO.BurnParam owner1 DAO.unfrozenTokenId 10)
    & expectCustomErrorUnit #nOT_ADMIN

  withSender (AddressResolved admin) $ call dao (Call @"Burn") (DAO.BurnParam owner1 DAO.unfrozenTokenId 100)
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 100, #present .! 10)

  withSender (AddressResolved admin) $ call dao (Call @"Burn") (DAO.BurnParam owner1 DAO.frozenTokenId 100)
    & expectCustomError #fA2_INSUFFICIENT_BALANCE (#required .! 100, #present .! 10)

  withSender (AddressResolved admin) $ call dao (Call @"Burn") (DAO.BurnParam owner1 DAO.unfrozenTokenId 10)
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 0
  withSender (AddressResolved admin) $ call dao (Call @"Burn") (DAO.BurnParam owner1 DAO.frozenTokenId 5)
  checkTokenBalance (DAO.frozenTokenId) dao owner1 5

mintScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, DAO.ParameterC param pm)
  => OriginateFn param m -> m ()
mintScenario originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn

  withSender (AddressResolved owner1) $ call dao (Call @"Mint") (DAO.MintParam owner1 DAO.unfrozenTokenId 10)
    & expectCustomErrorUnit #nOT_ADMIN

  withSender (AddressResolved admin) $ call dao (Call @"Mint") (DAO.MintParam owner1 DAO.unfrozenTokenId 100)
  checkTokenBalance (DAO.unfrozenTokenId) dao owner1 100
  withSender (AddressResolved admin) $ call dao (Call @"Mint") (DAO.MintParam owner1 DAO.frozenTokenId 50)
  checkTokenBalance (DAO.frozenTokenId) dao owner1 50

transferContractTokensScenario
  :: forall caps base m param pm
  . (MonadNettest caps base m, FA2.ParameterC param, DAO.ParameterC param pm)
  => OriginateFn param m -> m ()
transferContractTokensScenario originateFn = do
  ((owner1, _), _, dao, admin) <- originateFn
  ((target_owner1, _), (target_owner2, _), fa2Contract, _) <- originateFn
  let addParams = FA2.OperatorParam
        { opOwner = target_owner1
        , opOperator = toAddress dao
        , opTokenId = DAO.unfrozenTokenId
        }
  withSender (AddressResolved target_owner1) $ call fa2Contract (Call @"Update_operators") [FA2.AddOperator addParams]

  let transferParams = [ FA2.TransferItem
            { tiFrom = target_owner1
            , tiTxs = [ FA2.TransferDestination
                { tdTo = target_owner2
                , tdTokenId = DAO.unfrozenTokenId
                , tdAmount = 10
                } ]
            } ]
      param = DAO.TransferContractTokensParam
        { DAO.tcContractAddress = toAddress fa2Contract
        , DAO.tcParams = transferParams
        }

  withSender (AddressResolved owner1) $ call dao (Call @"Transfer_contract_tokens") param
    & expectCustomErrorUnit #nOT_ADMIN

  withSender (AddressResolved admin) $ call dao (Call @"Transfer_contract_tokens") param
  checkTokenBalance (DAO.unfrozenTokenId) fa2Contract target_owner1 90
  checkTokenBalance (DAO.unfrozenTokenId) fa2Contract target_owner2 110

