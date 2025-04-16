{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Data.Aeson (object, (.=))
import Lib (fetchAllWorkoutsGroupedWithLog)
import Data.ByteString.Lazy (ByteString)
import Web.Scotty.Internal.Types (ScottyException)
import Network.Wai.Middleware.Cors (cors, simpleCors)

main :: IO ()
main = scotty 3000 $ do
  -- Add CORS middleware to allow all origins
  middleware simpleCors

  get "/workouts" $ do
    startDateStr <- param "startDate" `rescue` (\(_ :: ScottyException) -> pure "")
    let logger = putStrLn
    result <- liftIO $ fetchAllWorkoutsGroupedWithLog startDateStr logger
    case result of
      Left err -> json $ object ["error" .= err]
      Right groupedWorkouts -> do
        setHeader "Content-Type" "application/json"
        raw groupedWorkouts