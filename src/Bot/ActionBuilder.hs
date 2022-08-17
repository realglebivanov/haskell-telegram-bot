module Bot.ActionBuilder (buildAction) where

import qualified Bot.CommandParser as CommandParser
import Control.Monad ((>=>))
import qualified Models.Action as Action
import qualified Models.Command as Command
import qualified Models.User as User
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API.Types
import Telegram.Bot.Simple

buildAction update = buildAction' (getUserInfo update) (getCommand update)

buildAction' (Just User {userId = (UserId userId), userUsername = (Just userName)}) (Just command) =
  Just
    Action.Command
      { Action.user = User.User {User.id = userId, User.name = userName},
        Action.command = command
      }
buildAction' _ _ = Just Action.InvalidCommand

getCommand update = (Telegram.extractUpdateMessage >=> messageText) update >>= CommandParser.parseCommand

getUserInfo = Telegram.extractUpdateMessage >=> messageFrom