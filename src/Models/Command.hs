module Models.Command where

import Data.Text
import Models.Book

data Command
  = List
  | Show (Key Book)
  | Delete (Key Book)
  | Create {author :: Text, name :: Text}
  | Update {key :: Key Book, newAuthor :: Text, newName :: Text}
  deriving (Show)
