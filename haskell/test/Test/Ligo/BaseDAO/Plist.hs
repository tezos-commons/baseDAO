-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Test.Ligo.BaseDAO.Plist
  ( test_Plist
  ) where

import Universum hiding (drop, swap, toList)

import Data.List ((!!))
import qualified Data.List as DL
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog.Gen.Tezos.Crypto (genSecretKey)
import Lorentz hiding (cast, concat, get, not)
import Morley.Tezos.Address (mkKeyAddress)
import Morley.Tezos.Crypto (toPublic)

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Proposal.Plist
import Test.Ligo.BaseDAO.Common (makeProposalKey, metadataSize)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

genProposalKeyList :: MonadGen m => Int -> m [ProposalKey]
genProposalKeyList atLeast = do
  i <- Gen.integral (Range.constant atLeast 10)
  addrs <- genSecretKey
    <&> (\secret -> mkKeyAddress . toPublic $ secret)
    & Gen.list (Range.linear atLeast i)

  pure $ (\addr ->
      let proposalMeta = lPackValueRaw @Natural 1
          metaSize = metadataSize proposalMeta
          param = ProposeParams
            { ppFrom = addr
            , ppFrozenToken = metaSize
            , ppProposalMetadata = proposalMeta
            }
      in makeProposalKey param
    ) <$> addrs

-- | Test functions that operate on `Maybe ProposalDoublyLinkedList`, and compare
-- its result to similar function that operate on `[ProposalKey]`.
test_Plist :: [TestTree]
test_Plist =
  [ testProperty "Conversion from and to `list`" $ withTests 50 $ property $ do
      keyList <- forAll $ genProposalKeyList 0

      keyList === (plistToList $ plistFromList keyList)

  , testProperty "Compare `mem` function result between list and plist" $ withTests 50 $ property $ do
      keyList <- forAll $ genProposalKeyList 1

      (i :: Int) <- forAll $ Gen.integral (Range.constant 0 (length keyList - 1))
      let k = keyList !! i

      (k `elem` keyList) === (plistMem k $ plistFromList keyList)

    , testProperty "Compare `insert` function result between list and plist" $ withTests 50 $ property $ do
      keyList <- forAll $ genProposalKeyList 0
      keyList2 <- forAll $ genProposalKeyList 1
      (i :: Int) <- forAll $ Gen.integral (Range.constant 0 (length keyList2 - 1))
      let k = keyList2 !! i

      (keyList <> [k]) === (plistToList $ plistInsert k $ plistFromList keyList)

    , testProperty "Compare `delete` function result between list and plist" $ withTests 50 $ property $ do
      keyList <- forAll $ genProposalKeyList 1

      (i :: Int) <- forAll $ Gen.integral (Range.constant 0 (length keyList - 1))
      let k = keyList !! i

      (DL.delete k keyList) === (plistToList $ plistDelete k $ plistFromList keyList)

    , testProperty "Compare `pop` function result between list and plist" $ withTests 50 $ property $ do
      keyList <- forAll $ genProposalKeyList 1

      let (pFirst, pRest) = plistPop $ plistFromList keyList
      let (lFirst, lRest) = (\case Just (f, r) -> (Just f, r); Nothing -> (Nothing, [])) $ DL.uncons $ keyList

      pFirst === lFirst
      (plistToList pRest) === lRest

  ]

