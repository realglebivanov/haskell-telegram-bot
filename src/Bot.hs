{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Bot where

import qualified Bot.ActionBuilder as ActionBuilder
import qualified Bot.Reply as Reply
import qualified Bot.StateMachine as StateMachine
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import qualified Data.Text as T
import qualified Models.Action as Action
import qualified Models.BotCommand as BotCommand
import qualified Models.Command as Command
import qualified Models.Command.Result as Result
import qualified Models.User as User
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
  ( BotApp (..),
    BotM (..),
    Eff,
    editUpdateMessage,
    getEnvToken,
    reply,
    replyOrEdit,
    replyText,
    startBot_,
    (<#),
  )
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.UpdateParser (mkParser, parseUpdate)

type HandleCommand = User.User -> Command.Command -> IO Result.Result

buildBot :: HandleCommand -> BotApp StateMachine.StateContainer Action.Action
buildBot handleCommand =
  BotApp
    { botInitialModel = StateMachine.initialState,
      botAction = handleUpdate,
      botHandler = buildHandleAction handleCommand,
      botJobs = []
    }

handleUpdate ::
  Telegram.Update ->
  StateMachine.StateContainer ->
  Maybe Action.Action
handleUpdate update model = parseUpdate (mkParser ActionBuilder.buildAction) update

buildHandleAction ::
  HandleCommand ->
  Action.Action ->
  StateMachine.StateContainer ->
  Eff Action.Action StateMachine.StateContainer
buildHandleAction _handleCommand Action.NoAction state = pure state
buildHandleAction _handleCommand (Action.InvalidCommand _errorText) state =
  state <# Reply.sendInvalidCommandReply
buildHandleAction handleCommand Action.Command {..} state =
  runAction $ StateMachine.transition state user command
  where
    runAction (Just (newState, newUserState)) =
      newState <# do
        runAction' (BotCommand.buildCommand newUserState command) newUserState
    runAction Nothing = state <# Reply.sendInvalidCommandReply
    runAction' (Just command) _ = do
      result <- liftIO $ handleCommand user command
      Reply.send user result
    runAction' Nothing newUserState = do
      Reply.simpleReply command newUserState

run :: HandleCommand -> IO ()
run handleCommand = do
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault $ buildBot handleCommand) env