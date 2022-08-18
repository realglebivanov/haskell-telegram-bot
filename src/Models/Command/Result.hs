module Models.Command.Result where

import Fmt
import Models.Book
import Models.Command

data Result
  = Show (Maybe Book)
  | List [Book]
  | Delete (Key Book)
  | Create Book
  | Update (Maybe Book)