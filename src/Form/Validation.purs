module Frontend.Form.Validation where

import Prelude

import Csl as CS
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))

type FormError = String

requiredText :: String -> Either String String
requiredText input
  | input == "" = Left "Required"
  | otherwise = Right input

bech32Format :: String -> Either String CS.Address
bech32Format = note "Invalid Bech32 address" <<< CS.address.fromBech32

bigNumFormat :: String -> Either String CS.BigNum
bigNumFormat = note "Invalid number" <<< CS.bigNum.fromStr

notLessThanBigNum :: String -> String -> CS.BigNum -> Either String CS.BigNum
notLessThanBigNum hint expected bnInput = note ("Value " <> CS.bigNum.toStr bnInput <> " is less than " <> hint <> " " <> expected) do
  bnExpected <- CS.bigNum.fromStr expected
  if CS.bigNum.lessThan bnInput bnExpected then Nothing
  else Just bnInput
