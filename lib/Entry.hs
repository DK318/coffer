{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( dateModified
  , fieldKeyAllowedChars
  , EntryTag
  , Entry, EntryConvertible (..), newEntry
  , path, masterField, fields
  , Field, FieldKey (..), newField, newFieldKey, getFieldKey, newEntryTag, getEntryTag
  , visibility, value, tags
  , FieldVisibility(..)
  )
where

import Control.Lens
import Data.Aeson qualified as A
import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Fmt (Buildable, build)
import GHC.Generics (Generic)

import Coffer.Path (EntryPath)

type DateTime = UTCTime

newtype FieldKey = FieldKey T.Text
  deriving stock (Generic, Show, Eq)
  deriving newtype Buildable
  deriving anyclass (Hashable, A.FromJSON, A.ToJSON, A.FromJSONKey, A.ToJSONKey)

newFieldKey :: Text -> Either Text FieldKey
newFieldKey t
  | T.null t =
      Left "Tags must contain at least 1 character"
  | T.any (`notElem` fieldKeyAllowedChars) t =
      Left $ "Tags can only contain the following characters: '" <> T.pack fieldKeyAllowedChars <> "'"
  | otherwise = Right $ FieldKey t

fieldKeyAllowedChars :: [Char]
fieldKeyAllowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;"

getFieldKey :: FieldKey -> T.Text
getFieldKey (FieldKey t) = t

newtype EntryTag = EntryTag T.Text
  deriving stock (Generic, Show, Eq)
  deriving newtype (Buildable, Ord, Hashable)
  deriving anyclass (A.ToJSON, A.FromJSON)

newEntryTag :: Text -> Either Text EntryTag
newEntryTag tag
  | T.null tag =
      Left "Tags must contain at least 1 character"
  | T.any (`notElem` allowedChars) tag =
      Left $ "Tags can only contain the following characters: '" <> T.pack allowedChars <> "'"
  | otherwise = Right $ EntryTag tag
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"

getEntryTag :: EntryTag -> T.Text
getEntryTag (EntryTag t) = t

data FieldVisibility = Public | Private
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance Buildable FieldVisibility where
  build = \case
    Public -> "public"
    Private -> "private"

instance A.ToJSON FieldVisibility where
  toJSON = \case
    Public -> A.toJSON @Text "public"
    Private -> A.toJSON @Text "private"
instance A.FromJSON FieldVisibility where
  parseJSON = A.withText "visibility" \case
    "public" -> pure Public
    "private" -> pure Private
    other -> fail $ "expecting either 'public' or 'private', but found: '" <> T.unpack other <> "'"

data Field =
  Field
  { _fDateModified :: DateTime
  , _visibility :: FieldVisibility
  , _value :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, A.FromJSON, A.ToJSON, A.FromJSONKey, A.ToJSONKey)

newField :: UTCTime -> T.Text -> Field
newField time value =
  Field
  { _fDateModified = time
  , _visibility = Private
  , _value = value
  }

makeLensesFor [("_value", "value"), ("_visibility", "visibility")] ''Field

data Entry =
  Entry
  { _path :: EntryPath
  , _eDateModified :: DateTime
  , _masterField :: Maybe FieldKey
  , _fields :: HS.HashMap FieldKey Field
  , _tags :: [EntryTag]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, A.FromJSON, A.ToJSON, A.FromJSONKey, A.ToJSONKey)

newEntry :: EntryPath -> UTCTime -> Entry
newEntry path time =
  Entry
  { _path = path
  , _eDateModified = time
  , _masterField = Nothing
  , _fields = HS.empty
  , _tags = []
  }

makeLensesFor [("_path", "path"), ("_masterField", "masterField"), ("_fields", "fields"), ("_tags", "tags")] ''Entry

class DateModified a where
  dateModified :: Lens' a DateTime

instance DateModified Entry where
  dateModified = lens _eDateModified (\e d -> e { _eDateModified = d } )

instance DateModified Field where
  dateModified = lens _fDateModified (\e d -> e { _fDateModified = d } )

class EntryConvertible a where
  entry :: Prism a a Entry Entry
