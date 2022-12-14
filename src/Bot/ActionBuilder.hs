module Bot.ActionBuilder (buildAction) where

import qualified Bot.CommandParser as CommandParser
import Control.Monad ((>=>))
import Data.Text (pack)
import qualified Models.Action as Action
import qualified Models.Command as Command
import qualified Models.User as User
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API.Types
import Telegram.Bot.Simple

buildAction update = buildAction' (getUserInfo update) (parseCommand update)

buildAction' (Just User {userId = (UserId userId), userUsername = (Just userName)}) (Right command) =
  Just
    Action.Command
      { Action.user = User.User {User.id = userId, User.name = userName},
        Action.command = command
      }
buildAction' _ (Left parseError) = Just . Action.InvalidCommand $ parseError
buildAction' _ (Right command) = Just . Action.InvalidCommand . pack . show $ command

getUserInfo = Telegram.extractUpdateMessage >=> messageFrom

parseCommand update = parseCommand' $ firstJust callbackQueryData' messageText'
  where
    parseCommand' (Just command) = CommandParser.parseCommand command
    parseCommand' _ = Left "Failed to parse message"
    callbackQueryData' = (Telegram.updateCallbackQuery >=> callbackQueryData) update
    messageText' = (Telegram.extractUpdateMessage >=> messageText) update

firstJust just@(Just _) _ = just
firstJust _ just@(Just _) = just
firstJust _ _ = Nothing
