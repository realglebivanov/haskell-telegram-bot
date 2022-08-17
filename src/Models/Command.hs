{-# LANGUAGE DataKinds #-}

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

-- type Update' = Update (Key Book) Text Text

-- setKey :: Update' -> Key Book -> Update'
setKey update@Update {key = key} newKey = update {key = newKey}