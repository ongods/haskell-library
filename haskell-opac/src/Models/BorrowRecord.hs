module Models.BorrowRecord where

import Data.Time (Day)

data BorrowRecord = BorrowRecord
  { borrowID :: Int,
    borrowUserID :: Int,
    borrowBookID :: Int,
    borrowDate :: Day,
    borrowDueDate :: Day,
    borrowReturnDate :: Maybe Day,
    borrowIsReturned :: Bool
  }
  deriving (Show, Eq)

isOverdue :: Day -> BorrowRecord -> Bool
isOverdue today r = not (borrowIsReturned r) && borrowDueDate r < today