{-# LANGUAGE DerivingStrategies
           , DeriveGeneric
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , GADTs
           , MultiParamTypeClasses
           , StandaloneDeriving
           , TemplateHaskell
           , TypeFamilies
           , QuasiQuotes
           , UndecidableInstances
 #-}
module DB where

import Database.Persist
import Database.Persist.TH
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

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
