module Models.Action where

import Data.Text (Text)
import Models.Book
import Models.Command
import Models.User

data Action = NoAction | InvalidCommand | Command {user :: User, command :: Command} deriving (Show)
