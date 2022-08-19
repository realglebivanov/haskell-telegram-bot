module Models.BotState where

import qualified Data.Text as T
import Database.Persist (Key)
import qualified Models.Book as Book

data BotState
  = Idle
  | Edit (Key Book.Book)
  | EditAuthorName (Key Book.Book) T.Text
  | New
  | NewAuthorName T.Text
  deriving (Show)
