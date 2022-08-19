module Models.BotCommand where

import Data.Text
import Models.Book
import qualified Models.BotState as BotState
import qualified Models.Command as Command

data BotCommand
  = List
  | Edit (Key Book)
  | New
  | TextInput Text
  | Show (Key Book)
  | Delete (Key Book)
  deriving (Show)

buildCommand state List = Just Command.List
buildCommand state (Show key) = Just $ Command.Show key
buildCommand state (Delete key) = Just $ Command.Delete key
buildCommand (BotState.EditAuthorName key authorName) (TextInput bookName) =
  Just
    Command.Update
      { Command.key = key,
        Command.newAuthor = authorName,
        Command.newName = bookName
      }
buildCommand (BotState.NewAuthorName authorName) (TextInput bookName) =
  Just
    Command.Create
      { Command.author = authorName,
        Command.name = bookName
      }
buildCommand state (Edit _) = Nothing
buildCommand state New = Nothing
buildCommand _ _ = Nothing
