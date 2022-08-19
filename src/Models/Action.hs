module Models.Action where

import qualified Data.Text as T
import Database.Persist.Postgresql (fromSqlKey)
import Models.Book
import Models.BotCommand
import Models.User

data Action
  = NoAction
  | InvalidCommand T.Text
  | Command {user :: User, command :: BotCommand}

instance Show Action where
  show (Command {command = List}) = "/list"
  show (Command {command = (Show key)}) = "/show " ++ show (fromSqlKey key)
  show (Command {command = (Delete key)}) = "/delete " ++ show (fromSqlKey key)
  show (Command {command = (Edit key)}) = "/edit " ++ show (fromSqlKey key)
  show (Command {command = New}) = "/new"
  show (Command {command = (TextInput text)}) = T.unpack text
  show NoAction = "/no_action"
  show (InvalidCommand text) = T.unpack $ "/invalid_command: " <> "\"" <> text <> "\""
