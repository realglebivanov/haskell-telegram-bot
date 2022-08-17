{-# LANGUAGE RecordWildCards #-}

module Program where

import qualified Bot
import Data.Text (unpack)
import Database.Persist.Class.PersistEntity (Entity, entityVal)
import qualified Models.Book as Book
import qualified Models.Command as Command
import qualified Models.Command.Result as Result
import qualified Models.User as User
import qualified Repository

main :: IO ()
main = Bot.run handleCommand
  where
    handleCommand User.User {} (Command.Show key) =
      Result.Show <$> Repository.getRecord key
    handleCommand User.User {} Command.List =
      Result.List <$> Repository.listRecords [] []
    handleCommand User.User {} (Command.Delete key) =
      Repository.deleteRecord key >> pure (Result.Delete key)
    handleCommand User.User {} (Command.Update {..}) =
      Result.Update <$> Repository.updateRecord key []
    handleCommand User.User {id = userId} (Command.Create {..}) =
      Repository.insertRecord book >> pure (Result.Create book)
      where
        book =
          Book.Book
            { Book.bookUserId = fromIntegral userId,
              Book.bookName = unpack name,
              Book.bookAuthor = unpack author
            }