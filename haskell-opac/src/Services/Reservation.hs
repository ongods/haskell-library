{-# LANGUAGE OverloadedStrings #-}

module Services.Reservation
  ( reserveBook
  , cancelReservation
  , getReservationsForUser
  ) where

import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Time (Day)
import App.Env (AppM, Env(..))
import App.Error (AppError(..))
import DB.Connection (withTransaction)
import Models.Book (Book(..))
import Models.Reservation (Reservation(..))
import Models.User (User(..))
import Queries.Reservation
  ( deleteReservation
  , getReservationByID
  , getReservationByUserAndBook
  , getReservationsByUser
  , insertReservation
  )

reserveBook :: User -> Book -> Day -> AppM (Either AppError ())
reserveBook user book today = do
  existing <- getReservationByUserAndBook (userID user) (bookID book)
  case existing of
    Just _ -> return $ Left (AlreadyExists "You have already reserved this book.")
    Nothing -> do
      env <- ask
      liftIO $ withTransaction (envConnection env) $ do
        runReaderT (insertReservation (userID user) (bookID book) today) env
      return $ Right ()

cancelReservation :: Int -> AppM (Either AppError ())
cancelReservation rid = do
  env <- ask
  mReservation <- liftIO $ runReaderT (getReservationByID rid) env
  case mReservation of
    Nothing -> return $ Left (NotFound "Reservation not found")
    Just _ -> do
      liftIO $ withTransaction (envConnection env) $ do
        runReaderT (deleteReservation rid) env
      return $ Right ()

getReservationsForUser :: Int -> AppM [Reservation]
getReservationsForUser = getReservationsByUser
