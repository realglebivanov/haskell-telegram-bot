{-# LANGUAGE RecordWildCards #-}

module Bot.Reply where

import qualified Data.Text as T
import Database.Persist
import Fmt
import qualified Models.Action as Action
import qualified Models.Book as Book
import qualified Models.BotCommand as BotCommand
import qualified Models.BotState as BotState
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
  replyOrEdit $ buildEditMessage "Actions: " [[buildBackToListButton user]]
  pure Action.NoAction
send user (Result.Show key (Just book)) = do
  replyText $ buildBookDescription book
  replyOrEdit $
    buildEditMessage
      "Actions: "
      [ [buildEditButton user key],
        [buildDeleteButton user key],
        [buildBackToListButton user]
      ]
  pure Action.NoAction
send user (Result.Delete _key) = do
  replyText "Book successfully deleted"
  replyOrEdit $ buildEditMessage "Actions: " [[buildBackToListButton user]]
  pure Action.NoAction
send _user (Result.Create book) = pure Action.NoAction
send _user (Result.Update book) = pure Action.NoAction

simpleReply BotCommand.New _ = do
  replyText "Enter book author"
  pure Action.NoAction
simpleReply (BotCommand.TextInput _) (BotState.NewAuthorName _) = do
  replyText "Enter book title"
  pure Action.NoAction
simpleReply (BotCommand.Edit _) _ = do
  replyText "Enter new book author"
  pure Action.NoAction
simpleReply (BotCommand.TextInput _) (BotState.EditAuthorName _ _) = do
  replyText "Enter new book title"
  pure Action.NoAction
simpleReply _ _ = sendInvalidCommandReply

buildEditMessage text buttons =
  (toEditMessage text)
    { editMessageReplyMarkup =
        Just $
          Telegram.SomeInlineKeyboardMarkup $
            Telegram.InlineKeyboardMarkup buttons
    }

buildEditButton user key =
  actionButton "Edit" (Action.Command user (BotCommand.Edit key))

buildDeleteButton user key =
  actionButton "Delete" (Action.Command user (BotCommand.Delete key))

buildBackToListButton user =
  actionButton "Back" (Action.Command {user = user, command = BotCommand.List})

buildShowButton user book =
  actionButton
    (buildBookDescription (entityVal book))
    Action.Command {user = user, command = BotCommand.Show $ entityKey book}

buildBookDescription Book.Book {..} =
  "Author: " +| bookAuthor |+ " Name: " +| bookName |+ ""