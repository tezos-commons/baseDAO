-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

-- | Common types between different DAO implementations.
module SMT.Model.BaseDAO.Proposal.Plist
  ( plistMem
  , plistPop
  , plistInsert
  , plistDelete

  , plistToList
  , plistFromList
  ) where

import Universum

import qualified Data.Map as Map

import Morley.Michelson.Typed.Haskell.Value (BigMap(..))

import Ligo.BaseDAO.Types

plistMem :: ProposalKey -> Maybe ProposalDoublyLinkedList -> Bool
plistMem key plistMb =
  case plistMb of
    Just ProposalDoublyLinkedList{..} ->
      (plFirst == key) ||
      (case Map.lookup (key, prev) (plMap & bmMap) of
          Nothing -> False
          Just _ -> True
      )
    Nothing -> False

plistPop :: Maybe ProposalDoublyLinkedList -> (Maybe ProposalKey, Maybe ProposalDoublyLinkedList)
plistPop plistMb =
  case plistMb of
    Nothing -> (Nothing, Nothing)
    Just plist ->
      let newPlist = case Map.lookup ((plist & plFirst), next) (plist & plMap & bmMap) of
            Just nextKey -> Just $ plist
              { plFirst = nextKey
              , plMap = (plist & plMap & bmMap)
                  & Map.delete ((plist & plFirst), next)
                  & Map.delete (nextKey, prev)
                  & BigMap Nothing
              }
            Nothing -> Nothing
      in (Just (plist & plFirst), newPlist)

plistInsert :: ProposalKey -> Maybe ProposalDoublyLinkedList -> Maybe ProposalDoublyLinkedList
plistInsert key plistMb =
  case plistMb of
    Nothing ->
      Just $ ProposalDoublyLinkedList { plFirst = key, plLast = key, plMap = mempty }
    Just plist ->
      Just $ plist
        { plLast = key
        , plMap = (plist & plMap & bmMap)
            & Map.insert (key, prev) (plist & plLast)
            & Map.insert ((plist & plLast), next) key
            & BigMap Nothing
        }

plistDelete :: ProposalKey -> Maybe ProposalDoublyLinkedList -> Maybe ProposalDoublyLinkedList
plistDelete key plistMb =
  case plistMb of
    Nothing ->
      error "BAD_STATE: no proposal"
    Just plist ->
      -- Special case: there is a single key.
      if (plist & plFirst) == (plist & plLast)
      then
        -- Either it's a different key or the list gets empty.
        if (key == (plist & plFirst)) then Nothing else Just plist
      else
        let
          -- We know that there are at least 2 keys in the list
          -- find the keys near to key we are looking for (if any):
          mPrevKey = Map.lookup (key, prev) (plist & plMap & bmMap)
          mNextKey = Map.lookup (key, next) (plist & plMap & bmMap)

          -- Remove both links from the map already:
          newMap = BigMap Nothing $ Map.delete (key, prev)
            (Map.delete (key, next) (plist & plMap & bmMap))

          -- Update the next link of the prev key and find the last key
          (newLast, newMap2) = case mPrevKey of
            Nothing ->
              -- There are no prev key, no changes needed
              ((plist & plLast), newMap)
            Just prevKey ->
              ( (if (plist & plLast) == key then prevKey else (plist & plLast))
              , BigMap Nothing $ Map.update (const mNextKey) (prevKey, next) (newMap & bmMap)
              )

          -- Update the prev link of the next key and find the first key
          (newFirst, newMap3) = case mNextKey of
            Nothing ->
              -- There are no next key, no changes needed
              ((plist & plFirst), newMap2)
            Just nextKey ->
              ( (if (plist & plFirst) == key then nextKey else (plist & plFirst))
              , BigMap Nothing $ Map.update (const mPrevKey) (nextKey, prev) (newMap2 & bmMap)
              )
        in
          Just $ ProposalDoublyLinkedList { plFirst = newFirst, plLast = newLast, plMap = newMap3 }


-----------------------------------------------------
-- Helper
-----------------------------------------------------

-- | Convert `Maybe ProposalDoublyLinkedList` to a list. Return empty list if input is `Nothing`.
plistToList :: Maybe ProposalDoublyLinkedList -> [ProposalKey]
plistToList plistMb =
  case plistMb of
    Nothing -> []
    Just plist -> plistMapToList plist (plist & plFirst) []
  where
    plistMapToList plist key result =
      case Map.lookup (key, next) (plist & plMap & bmMap) of
        Nothing -> result <> [key]
        Just nextKey -> plistMapToList plist nextKey (result <> [key])

-- | Convert a list of proposal_key into `Maybe ProposalDoublyLinkedList`.
-- Return `Nothing if input is an empty list.
plistFromList :: [ProposalKey] -> Maybe ProposalDoublyLinkedList
plistFromList list = foldl' (\acc el -> plistInsert el acc) Nothing list
