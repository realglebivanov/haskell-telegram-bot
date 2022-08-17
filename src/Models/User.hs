module Models.User where

import Data.Text (Text)

data User = User
  { id :: Integer,
    name :: Text
  }
  deriving (Show)
