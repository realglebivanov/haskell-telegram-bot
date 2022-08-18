module Models.Command.Result where

import Database.Persist (Entity)
import Fmt
import Models.Book
import Models.Command

data Result
  = Show (Key Book) (Maybe Book)
  | List [Entity Book]
  | Delete (Key Book)
  | Create Book
  | Update (Maybe Book)