-- User.hs

module UserModule where

data User = User
  { userId :: Int,
    name :: String,
    username :: String,
    password :: String
  }
  deriving (Show, Read)
