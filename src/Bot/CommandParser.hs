{-# LANGUAGE FlexibleContexts #-}

module Bot.CommandParser where

import Data.Text
import Data.Text.Read
import Database.Persist.Class.PersistEntity (Key)
import Database.Persist.Postgresql (toSqlKey)
import Models.Book
import Models.Command

parseCommand :: Text -> Maybe Command
parseCommand text = buildCommand $ splitOn " " text

buildCommand :: [Text] -> Maybe Command
buildCommand ["/list"] = Just List
buildCommand ["/show", id] = Show <$> buildKey id
buildCommand ["/delete", id] = Delete <$> buildKey id
buildCommand ["/update", id, author, name] =
  buildKey id >>= \key ->
    Just $
      Update
        { Models.Command.key = key,
          Models.Command.newAuthor = author,
          Models.Command.newName = name
        }
buildCommand _ = Nothing

buildKey :: Text -> Maybe (Key Book)
buildKey id = extractId $ decimal id
  where
    extractId (Right (id, "")) = Just $ toSqlKey id
    extractId _ = Nothing