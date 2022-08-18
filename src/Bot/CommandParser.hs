{-# LANGUAGE FlexibleContexts #-}

module Bot.CommandParser where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Database.Persist.Class.PersistEntity (Key)
import Database.Persist.Postgresql (toSqlKey)
import Models.Book
import Models.Command

parseCommand :: T.Text -> Either T.Text Command
parseCommand text = buildCommand $ T.splitOn " " text

buildCommand :: [T.Text] -> Either T.Text Command
buildCommand ["/list"] = Right List
buildCommand ["/show", id] = Show <$> buildKey id
buildCommand ["/delete", id] = Delete <$> buildKey id
buildCommand ["/create", author, name] =
  Right
    Create
      { Models.Command.author = author,
        Models.Command.name = name
      }
buildCommand ["/update", id, author, name] =
  buildKey id >>= \key ->
    Right
      Update
        { Models.Command.key = key,
          Models.Command.newAuthor = author,
          Models.Command.newName = name
        }
buildCommand text = Left $ T.unlines text

buildKey :: T.Text -> Either T.Text (Key Book)
buildKey id = extractId $ decimal id
  where
    extractId (Right (id, "")) = Right $ toSqlKey id
    extractId _ = Left $ T.unlines ["Failed to parse id ", id]