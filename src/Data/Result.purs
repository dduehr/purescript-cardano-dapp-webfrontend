module Frontend.Data.Result where

data Result a
  = Success a
  | Failure