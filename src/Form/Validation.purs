module Frontend.Form.Validation where

import Prelude

import Csl as CS
import Data.Either (Either, note)

type FormError = String

bech32Format :: String -> Either String CS.Address
bech32Format = note "Invalid Bech32 address" <<< CS.address.fromBech32

bigNumFormat :: String -> Either String CS.BigNum
bigNumFormat = note "Invalid number" <<< CS.bigNum.fromStr
