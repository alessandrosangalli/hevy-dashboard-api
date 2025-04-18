{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Data.Aeson (object, (.=))
import Lib.API (fetchAllWorkoutsGroupedWithLog)
import Data.ByteString.Lazy (ByteString)
import Web.Scotty.Internal.Types (ScottyException)
import Network.Wai.Middleware.Cors (cors, simpleCors)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import System.Environment (lookupEnv) -- Novo import
import Data.Maybe (fromMaybe) -- Novo import

main :: IO ()
main = do
  -- Lê a variável PORT, usa 3000 como fallback
  portStr <- fromMaybe "3000" <$> lookupEnv "PORT"
  let port = read portStr :: Int -- Converte para Int
  scotty port $ do
    -- Add CORS middleware
    middleware simpleCors

    get "/workouts" $ do
      startDateStr <- param "startDate" `rescue` (\(_ :: ScottyException) -> pure "")
      let logger :: Text -> IO ()
          logger = TextIO.putStrLn
      result <- liftIO $ fetchAllWorkoutsGroupedWithLog startDateStr logger
      case result of
        Left err -> json $ object ["error" .= err]
        Right groupedWorkouts -> do
          setHeader "Content-Type" "application/json"
          raw groupedWorkouts