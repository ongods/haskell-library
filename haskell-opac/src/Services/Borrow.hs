{-# LANGUAGE OverloadedStrings #-}

module Services.Borrow
  ( borrowBook
  , returnBook
  ) where

import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Time (Day)
import App.Env (AppM, Env(..))
import App.Error (AppError(..))
import DB.Connection (withTransaction)
import Models.Book (Book(..))
import Models.BorrowRecord (BorrowRecord(..))
import Models.User (User(..))
import Queries.Book (getBookByID, updateBookCopies)
import Queries.BorrowRecord (insertBorrowRecord, markAsReturned)

borrowBook :: User -> Book -> Day -> Day -> AppM (Either AppError ())
borrowBook user book borrowDate dueDate = do
  env <- ask
  if bookAvailableCopies book <= 0
    then return $ Left (ValidationError "This book is not currently available.")
    else liftIO $ withTransaction (envConnection env) $ do
      runReaderT action env
      return $ Right ()
  where
    action :: AppM ()
    action = do
      insertBorrowRecord borrowRecord
      updateBookCopies (bookID book) (bookAvailableCopies book - 1)

    borrowRecord =
      BorrowRecord
        { borrowID = 0
        , borrowUserID = userID user
        , borrowBookID = bookID book
        , borrowDate = borrowDate
        , borrowDueDate = dueDate
        , borrowReturnDate = Nothing
        , borrowIsReturned = False
        }

returnBook :: BorrowRecord -> Day -> AppM (Either AppError ())
returnBook record returnDate = do
  env <- ask
  mBook <- liftIO $ runReaderT (getBookByID (borrowBookID record)) env
  case mBook of
    Nothing -> return $ Left (DBError "Unable to return book because it no longer exists.")
    Just book -> liftIO $ withTransaction (envConnection env) $ do
      runReaderT (markAsReturned (borrowID record) returnDate >> updateBookCopies (bookID book) (bookAvailableCopies book + 1)) env
      return $ Right ()
