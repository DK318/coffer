module CLI.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Compat (Day, UTCTime , Year)
import Data.Time.Calendar.Month.Compat (Month)
import Entry (FieldKey, Field, Entry, FieldVisibility, EntryTag)
import Coffer.Directory (Directory)
import Coffer.Path (Path, EntryPath)

data Command res where
  CmdView :: ViewOptions -> Command ViewResult
  CmdCreate :: CreateOptions -> Command CreateResult
  CmdSetField :: SetFieldOptions -> Command SetFieldResult
  CmdDeleteField :: DeleteFieldOptions -> Command DeleteFieldResult
  CmdFind :: FindOptions -> Command (Maybe Directory)
  CmdRename :: RenameOptions -> Command RenameResult
  CmdCopy :: CopyOptions -> Command CopyResult
  CmdDelete :: DeleteOptions -> Command DeleteResult
  CmdTag :: TagOptions -> Command TagResult

deriving stock instance Show (Command res)

data SomeCommand where
  SomeCommand :: Command res -> SomeCommand

----------------------------------------------------------------------------
-- Command results
----------------------------------------------------------------------------

data ViewResult
  = VRDirectory Directory
  | VREntry Entry
  | VRField FieldKey Field
  | VRPathNotFound Path
  | VRDirectoryNoFieldMatch Path FieldKey
  | VREntryNoFieldMatch EntryPath FieldKey

data CreateResult
  = CRSuccess Entry
  | CREntryAlreadyExists EntryPath
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SetFieldResult
  = SFRSuccess Entry
  | SFREntryNotFound EntryPath
  | SFRMissingFieldContents EntryPath
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteFieldResult
  = DFRSuccess Entry
  | DFREntryNotFound EntryPath
  | DFRFieldNotFound FieldKey
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type RenameResult = CopyResult

data CopyResult
  = CPRSuccess [(EntryPath, EntryPath)]
  | CPRPathNotFound Path
  | CPRMissingEntryName
  | CPRDestinationIsDirectory [(EntryPath, EntryPath)]
  | CPREntryAlreadyExists [(EntryPath, EntryPath)]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteResult
  = DRSuccess [EntryPath]
  | DRPathNotFound Path
  | DRDirectoryFound Path
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TagResult
  = TRSuccess Entry
  | TREntryNotFound EntryPath
  | TRTagNotFound EntryTag
  | TRDuplicateTag EntryTag
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data ViewOptions = ViewOptions
  { voPath :: Path
  , voFieldName :: Maybe FieldKey
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateOptions = CreateOptions
  { coPath :: EntryPath
  , coEdit :: Bool
  , coForce :: Bool
  , coTags :: [EntryTag]
  , coFields :: [FieldInfo]
  , coPrivateFields :: [FieldInfo]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SetFieldOptions = SetFieldOptions
  { sfoPath :: EntryPath
  , sfoFieldName :: FieldKey
  , sfoFieldContents :: Maybe Text
  , sfoVisibility :: Maybe FieldVisibility
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteFieldOptions = DeleteFieldOptions
  { dfoPath :: EntryPath
  , dfoFieldName :: FieldKey
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FindOptions = FindOptions
  { foPath :: Maybe Path
  , foText :: Maybe Text
  , foSort :: Maybe (Sort, Direction)
  , foFilters :: [Filter]
  , foFilterFields :: [(FieldKey, FilterField)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RenameOptions = RenameOptions
  { roOldPath :: Path
  , roNewPath :: Path
  , roForce :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CopyOptions = CopyOptions
  { cpoOldPath :: Path
  , cpoNewPath :: Path
  , cpoForce :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteOptions = DeleteOptions
  { doPath :: Path
  , doRecursive :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TagOptions = TagOptions
  { toPath :: EntryPath
  , toTagName :: EntryTag
  , toDelete :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

----------------------------------------------------------------------------
-- Option arguments
----------------------------------------------------------------------------

data FieldInfo = FieldInfo
  { fiName :: FieldKey
  , fiContents :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Direction = Asc | Desc
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Sort
  = SortByEntryName
  | SortByEntryDate
  | SortByFieldValue FieldKey
  | SortByFieldDate FieldKey
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FilterOp = OpGT | OpGTE | OpLT | OpLTE | OpEQ
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FilterDate
  = FDYear Year
  | FDMonth Month
  | FDDay Day
  | FDTime UTCTime
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Filter
  = FilterByDate FilterOp FilterDate
  | FilterByName Text
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FilterField
  = FilterFieldByDate FilterOp FilterDate
  | FilterFieldByValue Text
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
