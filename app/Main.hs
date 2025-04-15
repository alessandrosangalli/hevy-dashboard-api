{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Lib (fetchAllWorkoutsGroupedWithLog)
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Web.Scotty.Internal.Types (ScottyException)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get "/workouts" $ do
    startDateParam <- param "startDate" `rescue` (\(_ :: ScottyException) -> pure "")
    let maybeStartDate = if startDateParam == ""
                         then Nothing
                         else parseTimeM True defaultTimeLocale "%Y-%m-%d" startDateParam :: Maybe UTCTime
    result <- liftIO $ fetchAllWorkoutsGroupedWithLog maybeStartDate putStrLn
    case result of
      Left err -> json $ object ["error" .= err]
      Right groupedWorkouts -> json groupedWorkouts