module DerParser.Token where

data Token = Token
  { tokenClass :: String
  , tokenValue :: String
  }
  deriving (Eq, Show)
