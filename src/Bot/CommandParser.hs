{-# LANGUAGE FlexibleContexts #-}

module Bot.CommandParser where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Database.Persist.Class.PersistEntity (Key)
import Database.Persist.Postgresql (toSqlKey)
import Models.Book
import qualified Models.BotCommand as BotCommand

parseCommand text = parseCommand' $ T.splitOn " " text

parseCommand' ["/list"] = Right BotCommand.List
parseCommand' ["/new"] = Right BotCommand.New
parseCommand' ["/edit", id] = BotCommand.Edit <$> parseKey id
parseCommand' ["/show", id] = BotCommand.Show <$> parseKey id
parseCommand' ["/delete", id] = BotCommand.Delete <$> parseKey id
parseCommand' text = Right . BotCommand.TextInput . T.intercalate " " $ text

parseKey :: T.Text -> Either T.Text (Key Book)
parseKey id = extractId $ decimal id
  where
    extractId (Right (id, "")) = Right $ toSqlKey id
    extractId _ = Left $ "Failed to parse id " <> id