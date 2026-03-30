{-# LANGUAGE OverloadedStrings #-}

module CLI.Repl
  ( startRepl
  ) where

import Control.Monad.IO.Class (liftIO)
import App.Env (AppM)
import App.Error (AppError(..))
import Models.User (User(..))
import CLI.Display (printHeader, printError, printSuccess)
import CLI.Prompt (selectFrom)
import CLI.User.Auth (registerScreen, loginScreen)

startRepl :: AppM ()
startRepl = mainMenuLoop

mainMenuLoop :: AppM ()
mainMenuLoop = do
  liftIO $ printHeader "Welcome to OPAC"
  choice <- liftIO $ selectFrom "Please select an option:"
    [ "Register"
    , "Login"
    , "Exit"
    ]
  case choice of
    Just 1  -> do
      registerScreen
      mainMenuLoop
    Just 2  -> do
      mUser <- loginScreen
      case mUser of
        Nothing   -> mainMenuLoop
        Just user -> userMenuLoop user
    Just 3  -> liftIO $ putStrLn "Goodbye!"
    _       -> mainMenuLoop

userMenuLoop :: User -> AppM ()
userMenuLoop user = do
  liftIO $ printHeader $ "Hello, " <> userFirstName user
  choice <- liftIO $ selectFrom "What would you like to do?"
    [ "Search Books"
    , "Borrow / Return"
    , "My Reservations"
    , "My Profile"
    , "Logout"
    ]
  case choice of
    Just 1  -> userMenuLoop user  -- placeholder, Search coming later
    Just 2  -> userMenuLoop user  -- placeholder, Borrow coming later
    Just 3  -> userMenuLoop user  -- placeholder, Reservations coming later
    Just 4  -> userMenuLoop user  -- placeholder, Profile coming later
    Just 5  -> mainMenuLoop
    _       -> userMenuLoop user