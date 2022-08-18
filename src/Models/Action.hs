{-# LANGUAGE RecordWildCards #-}

module Models.Action where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import Database.Persist.Postgresql (fromSqlKey)
import Models.Book
import Models.Command
import Models.User

data Action
  = NoAction
  | InvalidCommand T.Text
  | Command {user :: User, command :: Command}

instance Show Action where
  show (Command {command = List}) = "/list"
  show (Command {command = (Show key)}) = "/show " ++ show (fromSqlKey key)
  show (Command {command = (Delete key)}) = "/delete " ++ show (fromSqlKey key)
  show (Command {command = Update {..}}) =
    LazyT.unpack . LazyT.fromChunks $
      [ "/update ",
        T.pack . show $ fromSqlKey key,
        " ",
        newAuthor,
        " ",
        newName
      ]
  show (Command {command = Create {..}}) =
    LazyT.unpack . LazyT.fromChunks $ ["/create", author, name]
  show NoAction = "/no_action"
  show (InvalidCommand text) =
    LazyT.unpack . LazyT.fromChunks $ ["/invalid_command@", text]