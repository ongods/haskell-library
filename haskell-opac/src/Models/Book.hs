module Models.Book where

import Data.Text (Text)

data Book = Book
  { bookID :: Int,
    bookTitle :: Text,
    bookAuthor :: Text,
    bookPublication :: Text,
    bookISBN :: Text,
    bookGenre :: Text,
    bookAvailableCopies :: Int,
    bookTotalCopies :: Int
  }
  deriving (Show, Eq)