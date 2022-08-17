{-# LANGUAGE RecordWildCards #-}

module Bot where

import qualified Bot.ActionBuilder as ActionBuilder
import qualified Bot.ReplyBuilder as ReplyBuilder
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import qualified Models.Action as Action
import qualified Models.Command as Command
import qualified Models.Command.Result as Result
import qualified Models.User as User
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
  ( BotApp (..),
    BotM (..),
    Eff,
    getEnvToken,
    reply,
    replyText,
    startBot_,
    (<#),
  )
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.UpdateParser (mkParser, parseUpdate)

data Model = Model deriving (Show)

type HandleCommand = User.User -> Command.Command -> IO Result.Result

buildBot :: HandleCommand -> BotApp Model Action.Action
buildBot handleCommand =
  BotApp
    { botInitialModel = Model,
      botAction = handleUpdate,
      botHandler = buildHandleAction handleCommand,
      botJobs = []
    }

handleUpdate :: Telegram.Update -> Model -> Maybe Action.Action
handleUpdate update model = parseUpdate (mkParser ActionBuilder.buildAction) update

buildHandleAction :: HandleCommand -> Action.Action -> Model -> Eff Action.Action Model
buildHandleAction _handleCommand Action.NoAction model = pure model
buildHandleAction _handleCommand Action.InvalidCommand model =
  model <# do
    reply ReplyBuilder.buildInvalidCommandReply
    pure Action.NoAction
buildHandleAction handleCommand Action.Command {..} model =
  model <# do
    result <- liftIO $ handleCommand user command
    reply $ ReplyBuilder.buildReply result

run :: HandleCommand -> IO ()
run handleCommand = do
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault $ buildBot handleCommand) env