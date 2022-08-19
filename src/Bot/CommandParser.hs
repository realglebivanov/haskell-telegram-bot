{-# LANGUAGE FlexibleContexts #-}

module Bot.CommandParser where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Database.Persist.Class.PersistEntity (Key)
import Database.Persist.Postgresql (toSqlKey)
import Models.Book
import qualified Models.BotCommand as BotCommand

parseCommand text = buildCommand $ T.splitOn " " text

buildCommand ["/list"] = Right BotCommand.List
buildCommand ["/new"] = Right BotCommand.New
buildCommand ["/edit", id] = BotCommand.Edit <$> buildKey id
buildCommand ["/show", id] = BotCommand.Show <$> buildKey id
buildCommand ["/delete", id] = BotCommand.Delete <$> buildKey id
buildCommand text = Right . BotCommand.TextInput . T.intercalate " " $ text

buildKey :: T.Text -> Either T.Text (Key Book)
buildKey id = extractId $ decimal id
  where
    extractId (Right (id, "")) = Right $ toSqlKey id
    extractId _ = Left $ "Failed to parse id " <> id