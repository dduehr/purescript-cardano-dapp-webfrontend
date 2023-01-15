module Frontend.Capability.Domain.Contract where

import Prelude

import Data.Maybe (Maybe(..))

import Frontend.Api.Domain.Contract (RedeemAdaFromContractFields, RedeemTokenFromContractFields, SendAdaToContractFields, SendTokenToContractFields)
import Frontend.Api.LogMessages (class LogMessages, logMessage)
import Frontend.Data.Tx (TxId)
import Frontend.Data.Wallet (WalletApi)

sendAdaToContract
  :: ∀ m
   . Monad m
  => LogMessages m
  => WalletApi
  -> SendAdaToContractFields
  -> m (Maybe TxId)
sendAdaToContract _ _ = do
  logMessage "sendAdaToContract not implemented yet"
  pure Nothing

sendTokenToContract :: ∀ m. Monad m => WalletApi -> SendTokenToContractFields -> m (Maybe TxId)
sendTokenToContract _ _ = pure Nothing

redeemAdaFromContract :: ∀ m. Monad m => WalletApi -> RedeemAdaFromContractFields -> m (Maybe TxId)
redeemAdaFromContract _ _ = pure Nothing

redeemTokenFromContract :: ∀ m. Monad m => WalletApi -> RedeemTokenFromContractFields -> m (Maybe TxId)
redeemTokenFromContract _ _ = pure Nothing