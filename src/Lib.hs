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

module Lib
    ( initialize 
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.TH
import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto hiding (From, from, on)
import Database.Esqueleto.Experimental
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Handler sql=handlers
    codename Text    
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deriving Show
  Hitman sql=hitmen
    codename Text
    handlerId HandlerId
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deriving Show
  Mark sql=marks
    listBounty Int64
    firstName Text
    lastName Text
    description Text Maybe
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deriving Show
  PursuingMark sql=pursuing_marks
    hitmanId HitmanId 
    markId MarkId
    createdAt UTCTime
    updatedAt UTCTime Maybe
    Primary hitmanId markId
    deriving Show
  ErasedMark sql=erased_marks
    hitmanId HitmanId 
    markId MarkId
    awardedBounty Int64
    createdAt UTCTime
    updatedAt UTCTime Maybe
    Primary hitmanId markId
    deriving Show
|]

-- Get all the hitmen
getAllHitmen :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmen = select . from $ Table @Hitman

-- Get all the hitmen that are pursuing active marks (i.e. marks that haven’t been erased yet)
getAllHitmenPursuingActiveMarks :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmenPursuingActiveMarks = select $ do
  ((hitmen, pursuingMarks), erasedMarks) <- 
    from $ Table @Hitman
    `InnerJoin` Table @PursuingMark
    `on` (\(hitmen,pursuingMarks) ->
            pursuingMarks ^. PursuingMarkHitmanId ==. hitmen ^. HitmanId)
    `LeftOuterJoin` Table @ErasedMark
    `on` (\((_, pursuingMarks), erasedMarks) ->
           erasedMarks ?. ErasedMarkMarkId ==. just (pursuingMarks ^. PursuingMarkMarkId))
  where_ $ isNothing $ erasedMarks ?. ErasedMarkId
  pure hitmen


fromAllMarksErasedSince :: UTCTime -> SqlQuery (SqlExpr (Entity Mark), SqlExpr (Entity ErasedMark))
fromAllMarksErasedSince t = do
  (marks, erasedMarks) <- 
    from $ Table @Mark
    `InnerJoin` Table @ErasedMark
    `on` (\(marks, erasedMarks) ->
            erasedMarks ^. ErasedMarkMarkId ==. marks ^. MarkId)
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

--Get the total bounty awarded to a specific hitman
getTotalBountyAwardedToHitman :: MonadIO m => HitmanId -> SqlPersistT m [Value (Maybe Int64)]
getTotalBountyAwardedToHitman hitmanId = select $ do
  erasedMarks <- from $ Table @ErasedMark
  where_ $ erasedMarks ^. ErasedMarkHitmanId ==. val hitmanId
  pure (sum_ $ erasedMarks ^. ErasedMarkAwardedBounty)

--Get each hitman’s latest kill
getLatestKillForEachHitman :: MonadIO m => SqlPersistT m [(Entity Hitman, Maybe (Entity Mark))]
getLatestKillForEachHitman = undefined

--Get a specific hitman’s latest kill
getHitmansLatestKill :: MonadIO m => HitmanId -> SqlPersistT m (Maybe (Entity Mark))
getHitmansLatestKill hitmanId = undefined

--Get all the active marks that have only a single pursuer
getActiveMarksWithSinglePursuer :: MonadIO m => SqlPersistT m [Entity Mark]
getActiveMarksWithSinglePursuer = undefined 

--Get all the “marks of opportunity” (i.e. marks that a hitman erased without them marking the mark as being pursued first)
getMarksOfOpportunity :: MonadIO m => SqlPersistT m [Entity Mark]
getMarksOfOpportunity = select $ do
  ((marks, _), pursuingMarks) <- 
    from $ Table @Mark
    `InnerJoin` Table @ErasedMark
    `on` (\(marks, erasedMarks) -> 
            marks ^. MarkId ==. erasedMarks ^. ErasedMarkMarkId)
    `LeftOuterJoin` Table @PursuingMark
    `on` (\((_, erasedMarks), pursuingMarks) ->
            just (erasedMarks ^. ErasedMarkMarkId) ==. pursuingMarks ?. PursuingMarkMarkId)
  where_ $ isNothing $ pursuingMarks ?. PursuingMarkMarkId
  pure marks

initialize :: IO ()
initialize = runSqlite ":memory:" $ do
  printMigration migrateAll
  pure ()
