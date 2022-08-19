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

buildCommand _oldState _newState List = Just Command.List
buildCommand _oldState _newState (Show key) = Just $ Command.Show key
buildCommand _oldState _newState (Delete key) = Just $ Command.Delete key
buildCommand (BotState.EditAuthorName key authorName) BotState.Idle (TextInput bookName) =
  Just
    Command.Update
      { Command.key = key,
        Command.newAuthor = authorName,
        Command.newName = bookName
      }
buildCommand (BotState.NewAuthorName authorName) BotState.Idle (TextInput bookName) =
  Just
    Command.Create
      { Command.author = authorName,
        Command.name = bookName
      }
buildCommand _ _ _ = Nothing
