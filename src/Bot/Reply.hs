{-# LANGUAGE RecordWildCards #-}

module Bot.Reply where

import qualified Data.Text as T
import Database.Persist
import Fmt
import qualified Models.Action as Action
import qualified Models.Book as Book
import qualified Models.Command as Command
import qualified Models.Command.Result as Result
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple

sendInvalidCommandReply = do
  replyText "Invalid command given"
  pure Action.NoAction

send user (Result.List []) = do
  replyText "No books yet"
  pure Action.NoAction
send user (Result.List books) = do
  replyOrEdit $ buildEditMessage "Books: " (map ((: []) . buildShowButton user) books)
  pure Action.NoAction
send user (Result.Show _ Nothing) = do
  replyText "Couldn't find the book"
  replyOrEdit $ buildEditMessage "Actions: " [[backToListButton user]]
  pure Action.NoAction
send user (Result.Show key (Just book)) = do
  replyText $ buildBookDescription book
  replyOrEdit $ buildEditMessage "Actions: " [[deleteButton user key], [backToListButton user]]
  pure Action.NoAction
send user (Result.Delete _key) = do
  replyText "Book successfully deleted"
  replyOrEdit $ buildEditMessage "Actions: " [[backToListButton user]]
  pure Action.NoAction
send _user (Result.Create book) = pure Action.NoAction
send _user (Result.Update book) = pure Action.NoAction

buildEditMessage text buttons =
  (toEditMessage text)
    { editMessageReplyMarkup =
        Just $
          Telegram.SomeInlineKeyboardMarkup $
            Telegram.InlineKeyboardMarkup buttons
    }

deleteButton user key =
  actionButton "Delete" (Action.Command user (Command.Delete key))

backToListButton user =
  actionButton "Back" (Action.Command {user = user, command = Command.List})

buildShowButton user book =
  actionButton
    (buildBookDescription (entityVal book))
    Action.Command {user = user, command = Command.Show $ entityKey book}

buildBookDescription Book.Book {..} =
  "Author: " +| bookAuthor |+ " Name: " +| bookName |+ ""