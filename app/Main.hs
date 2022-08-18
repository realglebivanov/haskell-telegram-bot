{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Bot
import Data.Text (unpack)
import Database.Persist (Update (Update), (=.), (==.))
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
    handleCommand User.User {id = userId} Command.List =
      Result.List
        <$> Repository.listRecords
          [Book.BookUserId ==. fromIntegral userId]
          []
    handleCommand User.User {id = userId} (Command.Delete key) =
      Repository.deleteRecords
        [ Book.BookId ==. key,
          Book.BookUserId ==. fromIntegral userId
        ]
        >> pure (Result.Delete key)
    handleCommand User.User {id = userId} (Command.Update {..}) =
      Result.Update
        <$> ( Repository.updateRecords
                [ Book.BookId ==. key,
                  Book.BookUserId ==. fromIntegral userId
                ]
                [ Book.BookAuthor =. unpack newAuthor,
                  Book.BookName =. unpack newName
                ]
                >> Repository.getRecord key
            )
    handleCommand User.User {id = userId} (Command.Create {..}) =
      Repository.insertRecord book >> pure (Result.Create book)
      where
        book =
          Book.Book
            { Book.bookUserId = fromIntegral userId,
              Book.bookName = unpack name,
              Book.bookAuthor = unpack author
            }