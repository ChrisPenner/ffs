{-# LANGUAGE TemplateHaskell #-}
module Tags where

import Control.Lens hiding (Indexable)
import Data.IxSet

newtype Name = Name {unName :: String}
    deriving (Show, Eq, Ord)

data FType
    = TypeTag
    | TypeFile
    deriving (Show, Eq, Ord)

newtype Tag =
    Tag
        { unTag :: String
        }
    deriving (Show, Eq, Ord)

data TFile =
    TFile
        { _fileType :: FType
        , _name     :: Name
        , _tags     :: [Tag]
        }
    deriving (Show, Eq, Ord)

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
