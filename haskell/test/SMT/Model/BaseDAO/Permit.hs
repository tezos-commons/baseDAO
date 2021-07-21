-- SPDX-FileCopyrightText: 2021 TQ Tezos
-- SPDX-License-Identifier: LicenseRef-MIT-TQ

module SMT.Model.BaseDAO.Permit
 ( verifyPermitProtectedVote
 ) where

import Universum

import Control.Monad.Except (throwError)
import Lorentz hiding (not, cast, checkSignature, get)
import Tezos.Address (mkKeyAddress)
import Tezos.Crypto (checkSignature)

import Ligo.BaseDAO.Types
import SMT.Model.BaseDAO.Types

verifyPermitProtectedVote :: ModelSource -> PermitProtected VoteParam -> ModelT (VoteParam, Address)
verifyPermitProtectedVote mso permited = do
  let voteParam = permited & ppArgument
  case (permited & ppPermit) of
    Nothing ->
      -- if there is no permit to check, return the votingParam and the sender
      pure (permited & ppArgument, mso & msoSender)
    Just permit -> do
      -- if there is a permit, check that its correct
      permitCounter <- getStore <&> sPermitsCounter
      chainId_ <- get <&> msChainId
      selfAddr <- get <&> msSelfAddress
      -- verify that the received permit is correctly signed
      let dataToSign = lPackValueRaw @((ChainId, Address), (Nonce, VoteParam))
            ((chainId_, selfAddr),(permitCounter, voteParam))
          permitKey = permit & pKey
          permitSignature = permit & pSignature & unTSignature

      unless (checkSignature permitKey permitSignature dataToSign) $
        throwError MISSIGNED
      -- update the stored permit counter
      modifyStore $ \s -> pure $ s { sPermitsCounter = Nonce (unNonce permitCounter + 1) }
      -- return the voteParam with the permit sender
      pure (voteParam, mkKeyAddress (permit & pKey))
