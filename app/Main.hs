{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Lib (fetchAllWorkoutsWithLog)
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class ()

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get "/workouts" $ do
    result <- liftIO $ fetchAllWorkoutsWithLog putStrLn
    case result of
      Left err -> json $ object ["error" .= err]
      Right workouts -> json workouts