module Models.Reservation
  ( Reservation(..)
  ) where

import Data.Time (Day)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (toField)

data Reservation = Reservation
  { reservationID :: Int
  , reservationUserID :: Int
  , reservationBookID :: Int
  , reservationDate :: Day
  }
  deriving (Show, Eq)

instance FromRow Reservation where
  fromRow =
    Reservation
      <$> field
      <*> field
      <*> field
      <*> field

instance ToRow Reservation where
  toRow r =
    [ toField (reservationUserID r)
    , toField (reservationBookID r)
    , toField (reservationDate r)
    ]
