
module Web.Server where

import Data.Aeson
import Data.Proxy
import Data.Text

import Web.API
import CLI.Types
import Entry

import Servant.API
import Servant.Server

import Polysemy

makeServer
  :: forall impl m
  .  CofferBackend impl
  => (Command -> Sem m Value)
  -> ServerT (API impl) (Sem m)
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

view
  :: CofferBackend impl
  => (Command -> Sem m Value)
  -> Text
  -> Maybe FieldKey
  -> Sem m [Content impl]
view = error "view"

create
  :: (Command -> Sem m Value)
  -> Text
  -> Bool
  -> Bool
  -> [Text]
  -> [FieldKey]
  -> Text
  -> Sem m ()
create = error "create"

private :: (Command -> Sem m Value) -> Text -> Sem m NoContent
private = error "private"

public :: (Command -> Sem m Value) -> Text -> Sem m NoContent
public = error "public"

set :: (Command -> Sem m Value) -> Text -> Text -> Sem m ()
set = error "set"

deleteField :: (Command -> Sem m Value) -> Text -> Sem m NoContent
deleteField = error "deleteField"

find'
  :: (Command -> Sem m Value)
  -> Maybe Text
  -> Maybe Text
  -> Maybe Sort
  -> Maybe Filter
  -> Maybe FieldKey
  -> Maybe FieldKey
  -> Sem m [Content impl]
find' = error "find'"

rename
  :: (Command -> Sem m Value)
  -> Text
  -> Text
  -> Bool
  -> Sem m ()
rename = error "rename"

copy'
  :: (Command -> Sem m Value) -> Text -> Text -> Bool -> Sem m ()
copy' = error "copy'"

delete' :: (Command -> Sem m Value) -> Text -> Bool -> Sem m ()
delete' = error "delete'"

tag :: (Command -> Sem m Value) -> Text -> Text -> Bool -> Sem m ()
tag = error "tag"
