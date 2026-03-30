module DB.Schema where

import Data.String (fromString)
import Database.SQLite.Simple (Query)

createUsersTable :: Query
createUsersTable =
  fromString
    "CREATE TABLE IF NOT EXISTS users (\
    \ user_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ password_hash TEXT NOT NULL,\
    \ first_name TEXT NOT NULL,\
    \ last_name TEXT NOT NULL,\
    \ email TEXT NOT NULL UNIQUE,\
    \ birth_date TEXT NOT NULL,\
    \ occupation TEXT NOT NULL,\
    \ organization TEXT NOT NULL\
    \)"

createBooksTable :: Query
createBooksTable =
  fromString
    "CREATE TABLE IF NOT EXISTS books (\
    \ book_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ title TEXT NOT NULL,\
    \ author TEXT NOT NULL,\
    \ publication TEXT NOT NULL,\
    \ isbn TEXT NOT NULL UNIQUE,\
    \ genre TEXT NOT NULL,\
    \ available_copies INTEGER NOT NULL DEFAULT 0,\
    \ total_copies INTEGER NOT NULL DEFAULT 0\
    \)"

createBorrowRecordsTable :: Query
createBorrowRecordsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS borrow_records (\
    \ borrow_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ user_id INTEGER NOT NULL REFERENCES users(user_id),\
    \ book_id INTEGER NOT NULL REFERENCES books(book_id),\
    \ borrow_date TEXT NOT NULL,\
    \ due_date TEXT NOT NULL,\
    \ return_date TEXT,\
    \ is_returned INTEGER NOT NULL DEFAULT 0\
    \)"

createReservationsTable :: Query
createReservationsTable =
  fromString
    "CREATE TABLE IF NOT EXISTS reservations (\
    \ reservation_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ user_id INTEGER NOT NULL REFERENCES users(user_id),\
    \ book_id INTEGER NOT NULL REFERENCES books(book_id),\
    \ reservation_date TEXT NOT NULL\
    \)"
