-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Main
  ( main
  ) where

import Universum

import Cleveland.Ingredients (ourIngredients)
import Morley.Nettest.Tasty (nettestMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= nettestMainWithIngredients ourIngredients
