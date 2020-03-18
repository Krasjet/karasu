{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Models for database and requests, anything in this module will be exported
module Karasu.Models where

import Data.Text           (Text)
import Database.Persist.TH

type DocId = FilePath
type Version = Int
type MasterPassword = Text
type AccessCode = Text
type Markdown = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DocInfo
  docId DocId
  accCode AccessCode Maybe
  version  Version
  text  Markdown
  UniqueDocId docId
  deriving Show Eq
|]
