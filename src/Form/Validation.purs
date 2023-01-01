module Example.Form.Validation where

import Prelude

import Csl as Csl
import Data.Either (Either, note)

type FormError = String

bech32Format :: String -> Either String Csl.Address
bech32Format = note "Invalid Bech32 address" <<< Csl.address.fromBech32

bigNumFormat :: String -> Either String Csl.BigNum
bigNumFormat = note "Invalid number" <<< Csl.bigNum.fromStr
