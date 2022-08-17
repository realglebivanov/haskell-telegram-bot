module Bot.CommandParser where

import Data.Either.Combinators (rightToMaybe)
import Data.Text
import Data.Text.Read
import Database.Persist (PersistValue (PersistInt64), keyFromValues)
import Database.Persist.Class.PersistEntity (Key)
import Models.Book
import Models.Command

parseCommand :: Text -> Maybe Command
parseCommand text = buildCommand $ splitOn " " text

buildCommand :: [Text] -> Maybe Command
buildCommand ["list"] = Just List
buildCommand ["show", id] = Show <$> buildKey id
buildCommand ["delete", id] = Delete <$> buildKey id
buildCommand ["update", id, author, name] =
  Models.Command.setKey
    Update
      { Models.Command.newAuthor = author,
        Models.Command.newName = name
      }
    <$> buildKey id
buildCommand _ = Nothing

buildKey :: Text -> Maybe (Key Book)
buildKey id = extractId $ decimal id
  where
    extractId (Right (id, "")) = rightToMaybe $ keyFromValues [PersistInt64 id]
    extractId _ = Nothing