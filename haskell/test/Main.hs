-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module Main
  ( main
  ) where

import Universum

import Test.Cleveland.Ingredients (ourIngredients)
import Test.Cleveland.Tasty (clevelandMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= clevelandMainWithIngredients ourIngredients
