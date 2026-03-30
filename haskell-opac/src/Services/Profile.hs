{-# LANGUAGE OverloadedStrings #-}

module Services.Profile
  ( updateProfile
  , changePassword
  ) where

import Control.Monad.Reader (liftIO)
import Data.Text (Text)
import App.Env (AppM)
import App.Error (AppError(..))
import Models.User (User(..))
import Queries.User (updateUserPassword, updateUserProfile)
import Services.Auth (hashPassword, verifyPassword)

updateProfile :: User -> Text -> Text -> Text -> Text -> AppM (Either AppError User)
updateProfile user firstName lastName occupation organization = do
  let updatedUser = user
        { userFirstName = firstName
        , userLastName = lastName
        , userOccupation = occupation
        , userOrganization = organization
        }
  updateUserProfile updatedUser
  return $ Right updatedUser

changePassword :: User -> Text -> Text -> AppM (Either AppError User)
changePassword user currentPassword newPassword =
  if verifyPassword currentPassword (userPasswordHash user)
    then do
      result <- liftIO $ hashPassword newPassword
      case result of
        Left err -> return $ Left err
        Right hashed -> do
          updateUserPassword (userID user) hashed
          return $ Right user { userPasswordHash = hashed }
    else return $ Left (AuthError "Current password is incorrect")
