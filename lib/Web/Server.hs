
module Web.Server where

import Data.Aeson
import Data.Function ((&))
import Data.Text (Text)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)

import Web.API
import CLI.Types
import Entry

import Servant.API
import Servant.Server

import Polysemy

makeServer
  :: (Command -> Sem m Value)
  -> ServerT API (Sem m)
makeServer run
  =    view   run
  :<|> create run
  :<|> (\txt -> private run txt :<|> public run txt :<|> set run txt)
  :<|> deleteField run
  :<|> find' run
  :<|> rename run
  :<|> copy' run
  :<|> delete' run
  :<|> tag run

noContent :: Functor f => f a -> f NoContent
noContent = (NoContent <$)

type Confirmation = NoContent

view
  :: (Command -> Sem m Value)
  -> Text
  -> Maybe FieldKey
  -> Sem m Value
view run voPath voField = run $ CmdView ViewOptions {voPath, voField}

create
  :: (Command -> Sem m Value)
  -> Text
  -> Bool
  -> [Text]
  -> (HashMap FieldKey Text, HashMap FieldKey Text)
  -> Sem m Confirmation
create run coPath coForce coTags (fields, privateFields) =
  noContent $ run $ CmdCreate CreateOptions
    { coPath
    , coEdit = False
    , coForce
    , coTags
    , coFields        = fields        & HashMap.toList & map (uncurry FieldInfo)
    , coPrivateFields = privateFields & HashMap.toList & map (uncurry FieldInfo)
    }

private :: (Command -> Sem m Value) -> Text -> Sem m Confirmation
private run sfoPath =
  noContent $ run $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName = FieldKey ""
    , sfoFieldContents = ""
    , sfoVisibility = Just Private
    }

public :: (Command -> Sem m Value) -> Text -> Sem m Confirmation
public run sfoPath =
  noContent $ run $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName = FieldKey ""
    , sfoFieldContents = ""
    , sfoVisibility = Just Public
    }

set :: (Command -> Sem m Value) -> Text -> FieldKey -> Text -> Sem m Confirmation
set run sfoPath sfoFieldName sfoFieldContents =
  noContent $ run $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName
    , sfoFieldContents
    , sfoVisibility = Nothing
    }

deleteField :: (Command -> Sem m Value) -> Text -> FieldKey -> Sem m Confirmation
deleteField run dfoPath dfoFieldName =
  noContent $ run $ CmdDeleteField DeleteFieldOptions
    { dfoPath
    , dfoFieldName
    }

find'
  :: (Command -> Sem m Value)
  -> Maybe Text
  -> Maybe Text
  -> Maybe Sort
  -> Maybe SortField
  -> [Filter]
  -> [(FieldKey, FilterField)]
  -> Sem m Value
find' run foPath foText foSort foSortField foFilters foFilterFields =
  run $ CmdFind FindOptions
    { foPath
    , foText
    , foSort
    , foSortField
    , foFilters
    , foFilterFields
    }

rename
  :: (Command -> Sem m Value)
  -> Text
  -> Text
  -> Bool
  -> Sem m Confirmation
rename run roOldPath roNewPath roForce =
  noContent $ run $ CmdRename RenameOptions
    { roOldPath
    , roNewPath
    , roForce
    }

copy'
  :: (Command -> Sem m Value) -> Text -> Text -> Bool -> Sem m Confirmation
copy' run cpoOldPath cpoNewPath cpoForce =
  noContent $ run $ CmdCopy CopyOptions
    { cpoNewPath
    , cpoOldPath
    , cpoForce
    }

delete' :: (Command -> Sem m Value) -> Text -> Bool -> Sem m Confirmation
delete' run doPath doRecursive =
  noContent $ run $ CmdDelete DeleteOptions
    { doPath
    , doRecursive
    }

tag :: (Command -> Sem m Value) -> Text -> Text -> Bool -> Sem m Confirmation
tag run toPath toTagName toDelete =
  noContent $ run $ CmdTag TagOptions
    { toPath
    , toTagName
    , toDelete
    }
