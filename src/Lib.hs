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

import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import Data.Time.Clock (UTCTime)
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

getAllHitmen :: MonadIO m => SqlPersistT m [Entity Hitman]
getAllHitmen = select . from $ Table @Hitman

initialize :: IO ()
initialize = runSqlite ":memory:" $ do
  printMigration migrateAll
