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
  updateState newUserState
  where
    updateState (Just newUserState) =
      Just (StateContainer (M.insert userId newUserState states), oldUserState, newUserState)
    updateState Nothing = Nothing
    newUserState = transition' command oldUserState
    oldUserState = M.findWithDefault BotState.Idle userId states

transition' BotCommand.New BotState.Idle = Just BotState.New
transition' BotCommand.New _state = Nothing
transition' (BotCommand.Edit key) BotState.Idle = Just (BotState.Edit key)
transition' (BotCommand.Edit _key) _state = Nothing
transition' (BotCommand.TextInput text) (BotState.Edit key) = Just (BotState.EditAuthorName key text)
transition' (BotCommand.TextInput text) BotState.New = Just (BotState.NewAuthorName text)
transition' (BotCommand.TextInput _) (BotState.EditAuthorName _ _) = Just BotState.Idle
transition' (BotCommand.TextInput _) (BotState.NewAuthorName _) = Just BotState.Idle
transition' (BotCommand.TextInput _) _state = Nothing
transition' _command state = Just state
