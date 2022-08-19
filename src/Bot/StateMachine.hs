module Bot.StateMachine where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Models.Action as Action
import qualified Models.Book as Book
import qualified Models.BotCommand as BotCommand
import qualified Models.BotState as BotState
import qualified Models.User as User
import Telegram.Bot.Simple (BotM, Eff)

newtype StateContainer = StateContainer (M.Map Integer BotState.BotState)

instance Show StateContainer where
  show (StateContainer states) = concat . concatMap show' $ M.assocs states
    where
      show' (key, value) = [show key, ": ", show value, "\n"]

initialState = StateContainer M.empty

transition (StateContainer states) User.User {User.id = userId} command =
  updateState newState
  where
    newState = transition' command state
    state = M.lookup userId states
    updateState (Just newState) = Just (StateContainer $ M.insert userId newState states, newState)
    updateState Nothing = Nothing
    transition' command Nothing = transition' command (Just BotState.Idle)
    transition' BotCommand.New (Just BotState.Idle) = Just BotState.New
    transition' BotCommand.New (Just _state) = Nothing
    transition' (BotCommand.Edit key) (Just BotState.Idle) = Just (BotState.Edit key)
    transition' (BotCommand.Edit _key) (Just _state) = Nothing
    transition' (BotCommand.TextInput text) (Just (BotState.Edit key)) = Just (BotState.EditAuthorName key text)
    transition' (BotCommand.TextInput text) (Just BotState.New) = Just (BotState.NewAuthorName text)
    transition' (BotCommand.TextInput _) (Just (BotState.EditAuthorName _ _)) = Just BotState.Idle
    transition' (BotCommand.TextInput _) (Just (BotState.NewAuthorName _)) = Just BotState.Idle
    transition' (BotCommand.TextInput _) _state = Nothing
    transition' _command state@(Just _state) = state
