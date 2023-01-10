module Frontend.Api.Domain.Contract where

import Prelude

import Data.Maybe (Maybe(..))

import Frontend.Capability.Domain.Contract (RedeemAdaFromContractFields, RedeemTokenFromContractFields, SendAdaToContractFields, SendTokenToContractFields)
import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

sendAdaToContract :: ∀ m. Monad m => WalletApi -> SendAdaToContractFields -> m (Maybe TxId)
sendAdaToContract _ _ = pure Nothing

sendTokenToContract :: ∀ m. Monad m => WalletApi -> SendTokenToContractFields -> m (Maybe TxId)
sendTokenToContract _ _ = pure Nothing

redeemAdaFromContract :: ∀ m. Monad m => WalletApi -> RedeemAdaFromContractFields -> m (Maybe TxId)
redeemAdaFromContract _ _ = pure Nothing

redeemTokenFromContract :: ∀ m. Monad m => WalletApi -> RedeemTokenFromContractFields -> m (Maybe TxId)
redeemTokenFromContract _ _ = pure Nothing