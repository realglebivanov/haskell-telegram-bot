{-# LANGUAGE RecordWildCards #-}

module Bot.ReplyBuilder where

import Data.Text as T
import Fmt
import Models.Book
import Models.Command.Result
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple (toReplyMessage)

buildInvalidCommandReply = toReplyMessage "Invalid command given"

buildReply = toReplyMessage . buildReply'

buildReply' (Show Nothing) = "Couldn't find the book"
buildReply' (Show (Just book)) = buildBookDescription book
buildReply' (List books) = T.unlines $ Prelude.map buildBookDescription books
buildReply' (Delete _key) = "Show remaining books"
buildReply' (Create book) = buildBookDescription book
buildReply' (Update book) = buildBookDescription book

buildBookDescription Book {..} = "Author: " +| bookAuthor |+ "Name: " +| bookName |+ ""