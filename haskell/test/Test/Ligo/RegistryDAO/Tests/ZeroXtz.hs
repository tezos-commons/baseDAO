-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC
--
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
-- For all the incomplete list pattern matches in the calls to with
-- withOriginated func
module Test.Ligo.RegistryDAO.Tests.ZeroXtz
  ( zeroXtzTransfer
  ) where

import Prelude

import Test.Tasty (TestTree)

import Lorentz as L hiding (Contract, assert, div)
import Morley.Util.Named
import Test.Cleveland

import Ligo.BaseDAO.Types
import Test.Ligo.BaseDAO.Common
import Test.Ligo.Common
import Test.Ligo.RegistryDAO.Tests.Common
import Test.Ligo.RegistryDAO.Types

zeroXtzTransfer
  :: forall variant. RegistryTestConstraints variant => TestTree
zeroXtzTransfer = testScenario "proposal_check: fail when xtz transfer contains 0 mutez" $ scenario $
  withOriginated @variant 2
    (\_ s -> setVariantExtra @variant @"MinXtzAmount" (0 :: Mutez) s) $
    \(_:wallet1:_) fs baseDao _ -> do
      let proposalMeta = toProposalMetadata @variant $
              TransferProposal 1 [ xtzTransferType 0 wallet1 ] []
      let proposalSize = metadataSize proposalMeta
      withSender wallet1 $
        call baseDao (Call @"Freeze") (#amount :! proposalSize)

      startLevel <- getOriginationLevel' @variant baseDao

      -- Advance one voting period to a proposing stage.
      advanceToLevel (startLevel + toPeriod fs)

      withSender wallet1 $ call baseDao (Call @"Propose")
        (ProposeParams wallet1 proposalSize proposalMeta)
          & expectFailProposalCheck zeroMutezErrMsg
