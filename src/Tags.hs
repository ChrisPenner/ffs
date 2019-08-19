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
newTag s = TFile TypeTag (Name s)

newFile :: String -> [Tag] -> TFile
newFile s = TFile TypeFile (Name s)

makeLenses ''TFile

instance Indexable TFile where
  empty = ixSet [ ixFun (pure . _fileType)
                , ixFun (pure . _name)
                , ixFun (\f -> [Tag "/"] <> _tags f)
                ]

type TagMap = IxSet TFile
