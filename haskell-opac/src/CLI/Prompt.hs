{-# LANGUAGE OverloadedStrings #-}

module CLI.Prompt
  ( ask
  , askHidden
  , confirm
  , selectFrom
  , askDate
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hSetEcho, stdin, hFlush, stdout)
import Data.Time (Day, fromGregorianValid)

-- Ask for a text input
ask :: Text -> IO Text
ask prompt = do
  TIO.putStr $ prompt <> ": "
  hFlush stdout
  T.pack <$> getLine

-- Ask for a password (hidden input)
askHidden :: Text -> IO Text
askHidden prompt = do
  TIO.putStr $ prompt <> ": "
  hFlush stdout
  hSetEcho stdin False
  input <- getLine
  hSetEcho stdin True
  putStrLn ""
  return (T.pack input)

-- Ask for a yes/no confirmation
confirm :: Text -> IO Bool
confirm prompt = do
  TIO.putStr $ prompt <> " (y/n): "
  hFlush stdout
  input <- getLine
  return $ input `elem` ["y", "Y", "yes", "Yes"]

-- Display a numbered menu and get a selection
selectFrom :: Text -> [Text] -> IO (Maybe Int)
selectFrom prompt options = do
  TIO.putStrLn prompt
  mapM_ printOption (zip [1..] options)
  TIO.putStr "Choice: "
  hFlush stdout
  input <- getLine
  case reads input of
    [(n, "")] ->
      if n >= 1 && n <= length options
        then return (Just n)
        else do
          putStrLn "Invalid choice, try again."
          return Nothing
    _ -> do
      putStrLn "Please enter a number."
      return Nothing
  where
    printOption (i, opt) = TIO.putStrLn $ "  " <> T.pack (show i) <> ". " <> opt

-- Ask for a date
askDate :: Text -> IO Day
askDate prompt = do
  TIO.putStrLn prompt
  year <- askInt "Year (YYYY)"
  month <- askInt "Month (MM)"
  day <- askInt "Day (DD)"
  case fromGregorianValid (toInteger year) month day of
    Just d  -> return d
    Nothing -> do
      putStrLn "Invalid date provided. Please try again."
      askDate prompt

-- Internal helper for numeric input
askInt :: Text -> IO Int
askInt prompt = do
  TIO.putStr $ "  " <> prompt <> ": "
  hFlush stdout
  input <- getLine
  case reads input of
    [(n, "")] -> return n
    _ -> do
      putStrLn "Please enter a valid number."
      askInt prompt