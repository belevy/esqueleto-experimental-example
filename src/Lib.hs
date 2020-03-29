{-# LANGUAGE DerivingStrategies
           , DeriveGeneric
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , StandaloneDeriving
           , TemplateHaskell
           , TypeApplications
           , TypeFamilies
           , QuasiQuotes
           , UndecidableInstances
 #-}

module Lib where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Esqueleto hiding (From, from, on)
import qualified Database.Esqueleto as E
import Database.Esqueleto.Experimental
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Int
import Data.Maybe (listToMaybe)
import DB

-- Get all the hitmen
getAllHitmen :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmen = select . from $ Table @Hitman

getAllHitmenOld :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmenOld = select . E.from $ pure

-- Get all the hitmen that are pursuing active marks (i.e. marks that haven’t been erased yet)
getAllHitmenPursuingActiveMarks :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmenPursuingActiveMarks = select $ do
  hitmen :& pursuingMarks :& erasedMarks <-
    from $ Table @Hitman
    `InnerJoin` Table @PursuingMark
    `on` (\(hitmen :& pursuingMarks) ->
            pursuingMarks ^. PursuingMarkHitmanId ==. hitmen ^. HitmanId)
    `LeftOuterJoin` Table @ErasedMark
    `on` (\(_ :& pursuingMarks :& erasedMarks) ->
           erasedMarks ?. ErasedMarkMarkId ==. just (pursuingMarks ^. PursuingMarkMarkId))
  where_ $ isNothing $ erasedMarks ?. ErasedMarkId
  pure hitmen

getAllHitmenPursuingActiveMarksOld :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmenPursuingActiveMarksOld = select . E.from $ \(
                    hitmen
    `InnerJoin`     pursuingMarks
    `LeftOuterJoin` erasedMarks
  ) -> do
  E.on $ pursuingMarks ^. PursuingMarkHitmanId ==. hitmen ^. HitmanId
  E.on $ erasedMarks ?. ErasedMarkMarkId ==. just (pursuingMarks ^. PursuingMarkMarkId)
  where_ $ isNothing $ erasedMarks ?. ErasedMarkId
  pure hitmen


fromAllMarksErasedSince :: UTCTime -> SqlQuery (SqlExpr (Entity Mark), SqlExpr (Entity ErasedMark))
fromAllMarksErasedSince t = do
  marks :& erasedMarks <-
    from $ Table @Mark
    `InnerJoin` Table @ErasedMark
    `on` (\(marks :& erasedMarks) ->
            erasedMarks ^. ErasedMarkMarkId ==. marks ^. MarkId)
  where_ $ erasedMarks ^. ErasedMarkCreatedAt >=. val t
  pure (marks,erasedMarks)

fromAllMarksErasedSinceOld :: UTCTime -> SqlQuery (SqlExpr (Entity Mark), SqlExpr (Entity ErasedMark))
fromAllMarksErasedSinceOld t = E.from $ \(
                marks
    `InnerJoin` erasedMarks
  ) -> do
  E.on $ erasedMarks ^. ErasedMarkMarkId ==. marks ^. MarkId
  where_ $ erasedMarks ^. ErasedMarkCreatedAt >=. val t
  pure (marks,erasedMarks)

-- Get all the marks that have been erased since a given date
getAllMarksErasedSince :: MonadIO m => UTCTime -> SqlPersistT m [Entity Mark]
getAllMarksErasedSince t = select $ fst <$> fromAllMarksErasedSince t

-- Get all the marks that have been erased since a given date by a given hitman
getAllMarksErasedSinceBy :: MonadIO m => UTCTime -> HitmanId -> SqlPersistT m [Entity Mark]
getAllMarksErasedSinceBy t hitmanId = select $ do
  (marks, erasedMarks) <- fromAllMarksErasedSince t
  where_ $ erasedMarks ^. ErasedMarkHitmanId ==. val hitmanId
  pure marks

--Get the total bounty awarded to each hitman
getTotalBountyPerHitman :: MonadIO m => SqlPersistT m [(Value HitmanId, Value (Maybe Int64))]
getTotalBountyPerHitman = select $ do
  erasedMarks <- from $ Table @ErasedMark
  groupBy $ erasedMarks ^. ErasedMarkHitmanId
  pure (erasedMarks ^. ErasedMarkHitmanId, sum_ (erasedMarks ^. ErasedMarkAwardedBounty))

getTotalBountyPerHitmanOld :: MonadIO m => SqlPersistT m [(Value HitmanId, Value (Maybe Int64))]
getTotalBountyPerHitmanOld = select . E.from $ \erasedMarks -> do
  groupBy $ erasedMarks ^. ErasedMarkHitmanId
  pure (erasedMarks ^. ErasedMarkHitmanId, sum_ (erasedMarks ^. ErasedMarkAwardedBounty))

--Get the total bounty awarded to a specific hitman
getTotalBountyAwardedToHitman :: MonadIO m => HitmanId -> SqlPersistT m [Value (Maybe Int64)]
getTotalBountyAwardedToHitman hitmanId = select $ do
  erasedMarks <- from $ Table @ErasedMark
  where_ $ erasedMarks ^. ErasedMarkHitmanId ==. val hitmanId
  pure (sum_ $ erasedMarks ^. ErasedMarkAwardedBounty)

getTotalBountyAwardedToHitmanOld :: MonadIO m => HitmanId -> SqlPersistT m [Value (Maybe Int64)]
getTotalBountyAwardedToHitmanOld hitmanId = select . E.from $ \erasedMarks -> do
  where_ $ erasedMarks ^. ErasedMarkHitmanId ==. val hitmanId
  pure (sum_ $ erasedMarks ^. ErasedMarkAwardedBounty)

latestKillForEachHitman :: SqlQuery (SqlExpr (Entity Hitman), SqlExpr (Maybe (Entity Mark)))
latestKillForEachHitman = do
    hitmen :& _ :& erasedMarksLimit :& marks <-
      from $ Table @Hitman
      `LeftOuterJoin` Table @ErasedMark
      `on` (\(h :& em) ->
             just (h ^. HitmanId) ==. em ?. ErasedMarkHitmanId)
      `LeftOuterJoin` Table @ErasedMark
      `on` (\(_ :& em :& emLimit) ->
             emLimit ?. ErasedMarkHitmanId ==. (em ?. ErasedMarkHitmanId)
         &&. emLimit ?. ErasedMarkCreatedAt >. (em ?. ErasedMarkCreatedAt))
      `LeftOuterJoin` Table @Mark
      `on` (\(_ :& em :& _ :& marks) ->
              em ?. ErasedMarkMarkId ==. marks ?. MarkId)
    where_ $ isNothing $ erasedMarksLimit ?. ErasedMarkCreatedAt
    pure (hitmen, marks)

latestKillForEachHitmanOld :: SqlQuery (SqlExpr (Entity Hitman), SqlExpr (Maybe (Entity Mark)))
latestKillForEachHitmanOld = E.from $ \(
                    hitmen
    `LeftOuterJoin` em
    `LeftOuterJoin` emLimit
    `LeftOuterJoin` marks
  ) -> do
  E.on $ just (hitmen ^. HitmanId) ==. em ?. ErasedMarkHitmanId
  E.on $ emLimit ?. ErasedMarkHitmanId ==. (em ?. ErasedMarkHitmanId)
     &&. emLimit ?. ErasedMarkCreatedAt >. (em ?. ErasedMarkCreatedAt)
  E.on $ em ?. ErasedMarkMarkId ==. marks ?. MarkId
  where_ $ isNothing $ emLimit ?. ErasedMarkCreatedAt
  pure (hitmen, marks)

--Get each hitman’s latest kill
getLatestKillForEachHitman :: MonadIO m => SqlPersistT m [(Entity Hitman, Maybe (Entity Mark))]
getLatestKillForEachHitman = select latestKillForEachHitman

--Get a specific hitman’s latest kill
getHitmansLatestKill :: MonadIO m => HitmanId -> SqlPersistT m (Maybe (Entity Mark))
getHitmansLatestKill hitmanId = fmap (join . listToMaybe) $
  select $ do
  (hitmen, marks) <- latestKillForEachHitman
  where_ $ hitmen ^. HitmanId ==. val hitmanId
  pure marks

--Get all the active marks that have only a single pursuer
getActiveMarksWithSinglePursuer :: MonadIO m => SqlPersistT m [Entity Mark]
getActiveMarksWithSinglePursuer = select $ do
  (marks :& erasedMarks) <-
    from $ Table @Mark
    `LeftOuterJoin` Table @ErasedMark
    `on` (\(marks :& erasedMarks) ->
           erasedMarks ?. ErasedMarkMarkId ==. just (marks ^. MarkId))
  where_ $ isNothing (erasedMarks ?. ErasedMarkMarkId)
       &&. marks ^. MarkId `in_` subSelectList allMarksWithSinglePursuer
  pure marks

  where
    allMarksWithSinglePursuer = do
      pursuingMark <- from $ Table @PursuingMark
      having $ count (pursuingMark ^. PursuingMarkHitmanId) ==. val @Int 1
      groupBy $ pursuingMark ^. PursuingMarkMarkId
      pure $ pursuingMark ^. PursuingMarkMarkId

getActiveMarksWithSinglePursuerWithSubqueryJoin :: MonadIO m => SqlPersistT m [Entity Mark]
getActiveMarksWithSinglePursuerWithSubqueryJoin = select $ do
  (marks :& _ :& erasedMarks) <-
    from $ Table @Mark
    `InnerJoin` SelectQuery allMarksWithSinglePursuer
    `on` (\(marks :& spMarks) ->
           spMarks ==. marks ^. MarkId)
    `LeftOuterJoin` Table @ErasedMark
    `on` (\(marks :& _ :& erasedMarks) ->
           erasedMarks ?. ErasedMarkMarkId ==. just (marks ^. MarkId))
  where_ $ isNothing (erasedMarks ?. ErasedMarkMarkId)
  pure marks

  where
    allMarksWithSinglePursuer = do
      pursuingMark <- from $ Table @PursuingMark
      having $ count (pursuingMark ^. PursuingMarkHitmanId) ==. val @Int 1
      groupBy $ pursuingMark ^. PursuingMarkMarkId
      pure $ pursuingMark ^. PursuingMarkMarkId

getActiveMarksWithSinglePursuerOld :: MonadIO m => SqlPersistT m [Entity Mark]
getActiveMarksWithSinglePursuerOld = select . E.from $ \(
                    marks
    `LeftOuterJoin` erasedMarks
  ) -> do
  E.on $ erasedMarks ?. ErasedMarkMarkId ==. just (marks ^. MarkId)
  where_ $ isNothing (erasedMarks ?. ErasedMarkMarkId)
       &&. marks ^. MarkId `in_` subSelectList allMarksWithSinglePursuer
  pure marks

  where
    allMarksWithSinglePursuer = E.from $ \pursuingMark -> do
      having $ count (pursuingMark ^. PursuingMarkHitmanId) ==. val @Int 1
      groupBy $ pursuingMark ^. PursuingMarkMarkId
      pure $ pursuingMark ^. PursuingMarkMarkId

--Get all the “marks of opportunity” (i.e. marks that a hitman erased without them marking the mark as being pursued first)
getMarksOfOpportunity :: MonadIO m => SqlPersistT m [Entity Mark]
getMarksOfOpportunity = select $ do
  (marks :& _ :& pursuingMarks) <-
    from $ Table @Mark
    `InnerJoin` Table @ErasedMark
    `on` (\(marks :& erasedMarks) ->
            marks ^. MarkId ==. erasedMarks ^. ErasedMarkMarkId)
    `LeftOuterJoin` Table @PursuingMark
    `on` (\(_ :& erasedMarks :& pursuingMarks) ->
            just (erasedMarks ^. ErasedMarkMarkId) ==. pursuingMarks ?. PursuingMarkMarkId)
  where_ $ isNothing $ pursuingMarks ?. PursuingMarkMarkId
  pure marks

getMarksOfOpportunityOld :: MonadIO m => SqlPersistT m [Entity Mark]
getMarksOfOpportunityOld = select . E.from $ \(
                    marks
    `InnerJoin`     erasedMarks
    `LeftOuterJoin` pursuingMarks
  ) -> do
  E.on $ marks ^. MarkId ==. erasedMarks ^. ErasedMarkMarkId
  E.on $ just (erasedMarks ^. ErasedMarkMarkId) ==. pursuingMarks ?. PursuingMarkMarkId
  where_ $ isNothing $ pursuingMarks ?. PursuingMarkMarkId
  pure marks

initialize :: IO ()
initialize = do
    putStrLn "Executable not implemented"
