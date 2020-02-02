{-# LANGUAGE OverloadedStrings #-}

import Lib
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite)
import Data.Time.Clock

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  let dt1 = read "2017-01-01 12:30:00Z"
  handler1 <- insert $ Handler "Olive" dt1 Nothing
  handler2 <- insert $ Handler "Pallas" dt1 Nothing
  hitman1 <- insert $ Hitman "Callaird" handler1 dt1 Nothing
  hitman2 <- insert $ Hitman "Bomois" handler1 dt1 Nothing
  hitman3 <- insert $ Hitman "Dune" handler2 dt1 Nothing
  mark1 <- insert $ Mark 25000 "John" "Tosti" Nothing dt1 Nothing
  mark2 <- insert $ Mark 50000 "Macie" "Jordan" Nothing dt1 Nothing
  mark3 <- insert $ Mark 33000 "Sal" "Aspot" Nothing dt1 Nothing
  mark4 <- insert $ Mark 10000 "Lars" "Andersen" Nothing  dt1 Nothing
  pm1 <- insert $ PursuingMark hitman1 mark2 (UTCTime (read "2018-07-01") 0) Nothing
  pm2 <- insert_ $ PursuingMark hitman2 mark2 (UTCTime (read "2018-07-02") 0) Nothing
  pm3 <- insert_ $ PursuingMark hitman2 mark4 (UTCTime (read "2018-05-05") 0) Nothing
  pm4 <- insert_ $ PursuingMark hitman3 mark3 (UTCTime (read "2018-05-13") 0) Nothing
  pm5 <- insert_ $ PursuingMark hitman3 mark2 (UTCTime (read "2018-02-15") 0) Nothing
  em1 <- insert_ $ ErasedMark hitman1 mark2 30000 (UTCTime (read "2018-09-03") 0) Nothing
  em2 <- insert_ $ ErasedMark hitman1 mark1 55000 (UTCTime (read "2019-02-02") 0) Nothing
  em3 <- insert_ $ ErasedMark hitman3 mark3 27000 (UTCTime (read "2018-06-30") 0) Nothing
  pure ()
