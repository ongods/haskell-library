module Models.User where

import Data.Text (Text)
import Data.Time (Day)

data User = User
  { userID :: Int,
    userPasswordHash :: Text,
    userFirstName :: Text,
    userLastName :: Text,
    userEmail :: Text,
    userBirthDate :: Day,
    userOccupation :: Text,
    userOrganization :: Text
  }
  deriving (Show, Eq)