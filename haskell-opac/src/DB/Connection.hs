module DB.Connection
  ( withConnection,
    withTransaction,
  )
where

import Control.Exception (bracket)
import Database.SQLite.Simple (Connection, close, open)
import qualified Database.SQLite.Simple as SQL

withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection dbPath = bracket (open dbPath) close

withTransaction :: Connection -> IO a -> IO a
withTransaction conn action = SQL.withTransaction conn action