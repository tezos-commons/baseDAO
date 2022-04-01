-- SPDX-FileCopyrightText: 2021 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Plist.Property
  ( hprop_Plist
  ) where

import Universum hiding (drop, swap)
import qualified Unsafe ((!!))

import qualified Data.List as DL
import Fmt (build, unlinesF)
import Hedgehog hiding (assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lorentz hiding (abs, assert, and, div, not, now, (>>))
import Test.Cleveland

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Proposal.Plist
import Test.Ligo.BaseDAO.Plist (genProposalKeyList)
import Test.Plist.Type
import Test.Plist.Contract


genPlistParameter :: MonadGen m => [ProposalKey] -> [ProposalKey] -> m PlistParameter
genPlistParameter keyList keyList2 = do
  requireExistingKeyEps <-
    if (not $ null keyList) then do
      (i :: Int) <- Gen.integral (Range.constant 0 (length keyList - 1))
      let k = keyList Unsafe.!! i
      pure [ Mem k
           , Delete k
           ]
    else pure []

  notRequireExistingKeyEps <-
    if (not $ null keyList2) then do
      (i2 :: Int) <- Gen.integral (Range.constant 0 (length keyList2 - 1))
      let k2 = keyList2 Unsafe.!! i2
      pure [ Insert k2
           , Mem k2
           , Delete k2
           ]
    else pure []

  Gen.choice $ pure <$> (requireExistingKeyEps <> notRequireExistingKeyEps <> [Pop ()])

genPlistParameters :: MonadGen m => m ([ProposalKey], [PlistParameter])
genPlistParameters = do
  initSize <- Gen.integral (Range.constant 0 30)
  initSize2 <- Gen.integral (Range.constant 0 30)
  -- Init List to be used as storage
  keyList <- genProposalKeyList initSize
  -- A list to pick new key from
  keyList2 <- genProposalKeyList initSize2
  params <- Gen.list (Range.linear 10 20) $ genPlistParameter keyList keyList2
  pure (keyList, params)


hprop_Plist :: Property
hprop_Plist =
  withTests 100 $ property $ do
    plistTests

plistTests :: PropertyT IO ()
plistTests = do
  (keyList, params) <- forAll genPlistParameters
  testScenarioProps $
    scenarioEmulated $ do
      let defaultStore = PlistStorage (plistFromList keyList) False Nothing
      let haskellStore = (keyList, False, Nothing)
      -- Originate plist contract
      plistContract <- originateTypedSimple @PlistParameter @_ @() "PlistContract" defaultStore plistContractLigo
      callContractLoop params (TAddress $ chAddress plistContract) haskellStore

callContractLoop :: MonadEmulated caps m => [PlistParameter] -> TAddress PlistParameter () -> ([ProposalKey], Bool, Maybe ProposalKey) -> m ()
callContractLoop param addr haskellStore =
  case param of
    p:ps -> do
      let newHaskellStore = callHaskell haskellStore p
      newLigoStore <- callLigo addr p
      assert (newHaskellStore == newLigoStore) $
        unlinesF
          [ "━━ Error: Haskell and Ligo storage are different ━━"
          , build p
          , "━━ Haskell storage ━━"
          , build newHaskellStore
          , "━━ Ligo storage ━━"
          , build newLigoStore
          ]
      callContractLoop ps addr newHaskellStore
    [] -> pure ()

-- | Call haskell implementation which uses normal list.
callHaskell :: ([ProposalKey], Bool, Maybe ProposalKey) -> PlistParameter -> ([ProposalKey], Bool, Maybe ProposalKey)
callHaskell (keyList, memResult, popResult) param =
  case param of
    Insert k ->
      let newKeyList = if not (k `elem` keyList)
            then keyList <> [k]
            else keyList
      in (newKeyList, memResult, popResult)
    Delete k -> (DL.delete k keyList, memResult, popResult)
    Mem k -> (keyList, k `elem` keyList, popResult)
    Pop _ ->
      let (list, newPopResult) = case DL.uncons keyList of
            Just (h, r) -> (r, Just h)
            Nothing -> ([], Nothing)
      in (list, memResult, newPopResult)

-- | Call ligo plist contract.
callLigo :: MonadEmulated caps m => TAddress PlistParameter () -> PlistParameter -> m ([ProposalKey], Bool, Maybe ProposalKey)
callLigo addr param = do
  nettestResult <- attempt @TransferFailure $ case param of
    Insert k -> do
      PlistStorage{..} <- getFullStorage @PlistStorage (unTAddress addr)

      -- Ensure that the key being inserted does not exist in the list. In the main contract, this is checked by
      -- `check_if_proposal_exist`.
      if (k `elem` plistToList psPlist) then
        pure ()
      else
        call addr (Call @"Insert") k
    Delete k -> call addr (Call @"Delete") k
    Mem k -> call addr (Call @"Mem") k
    Pop _ -> call addr (Call @"Pop") ()

  case nettestResult of
    Right _ -> do
      PlistStorage{..} <- getFullStorage @PlistStorage (unTAddress addr)
      pure (plistToList psPlist, psMemResult, psPopResult)
    Left err -> error $ show err
