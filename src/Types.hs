{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Control.Lens hiding (Indexable)
import Data.IxSet
import Data.SafeCopy
import Data.Serialize
import GHC.Generics

data Env = Env {_realRoot :: String, _persistDir :: String}
makeLenses ''Env

newtype Name = Name {unName :: String}
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialize, SafeCopy)

data FType
    = TypeTag
    | TypeFile
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialize, SafeCopy)

newtype Tag =
    Tag
        { unTag :: String
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialize, SafeCopy)

data TFile =
    TFile
        { _fileType :: FType
        , _name     :: Name
        , _tags     :: [Tag]
        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialize, SafeCopy)

newTag :: String -> [Tag] -> TFile
newTag s tags = TFile TypeTag (Name s) (Tag "/" : tags)

newFile :: String -> [Tag] -> TFile
newFile s tags = TFile TypeFile (Name s) (Tag "/" : tags)

makeLenses ''TFile

instance Indexable TFile where
  empty = ixSet [ ixFun (pure . _fileType)
                , ixFun (pure . _name)
                , ixFun (\f -> _tags f)
                ]

type TagMap = IxSet TFile
