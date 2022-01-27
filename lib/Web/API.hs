{-# language NoMonomorphismRestriction #-}

module Web.API where

import Data.Aeson
import Data.Proxy
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as Text

import Servant.API

import CLI.Types
import Entry

type API impl
  = "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParam "path"  Text
      :> OptionalParam "field" FieldKey
      :> Get '[JSON] [Content impl]

    :<|> "create"
      :> RequiredParam  "path" Text
      :> QueryFlag      "force"
      :> QueryFlag      "private"
      :> OptionalParams "tag"   Text
      :> OptionalParams "field" FieldKey
      :> ReqBody '[JSON] Text
      :> Post '[JSON] ()

    :<|> "set-field"
      :> RequiredParam "path" Text
      :>
        (    "private" :> PostNoContent
        :<|> "public"  :> PostNoContent
        :<|> ReqBody '[JSON] Text
          :> Post '[JSON] ()
        )

    :<|> "delete-field"
      :> RequiredParam "path" Text
      :> DeleteNoContent

    :<|> "find"
      :> OptionalParam "path" Text
      :> OptionalParam "text" Text
      :> OptionalParam "sort" Sort
      :> OptionalParam "filter" Filter
      :> OptionalParam "filter-field" FieldKey
      :> OptionalParam "sort-field" FieldKey
      :> Get '[JSON] [Content impl]

    :<|> "rename"
      :> RequiredParam "old-path" Text
      :> RequiredParam "new-path" Text
      :> QueryFlag     "force"
      :> Post '[JSON] ()

    :<|> "copy"
      :> RequiredParam "old-path" Text
      :> RequiredParam "new-path" Text
      :> QueryFlag     "force"
      :> Post '[JSON] ()

    :<|> "delete"
      :> RequiredParam "path" Text
      :> QueryFlag     "recursive"
      :> Post '[JSON] ()

    :<|> "tag"
      :> RequiredParam "path" Text
      :> RequiredParam "tag-name" Text
      :> QueryFlag     "delete"
      :> Post '[JSON] ()
    )

type RequiredParam  = QueryParam' [Required, Strict]
type OptionalParam  = QueryParam
type OptionalParams = QueryParams

class
  ( FromJSON (Content impl)
  , ToJSON   (Content impl)
  )
  => CofferBackend impl
  where
    data Content impl :: Type

deriving newtype instance ToHttpApiData FieldKey

instance ToHttpApiData Sort where
  toUrlPiece (Sort sm dir) = toUrlPiece sm <> ":" <> toUrlPiece dir

instance ToHttpApiData SortMeans where
  toUrlPiece SMDate = "date"
  toUrlPiece SMName = "name"

instance ToHttpApiData Direction where
  toUrlPiece Asc  = "asc"
  toUrlPiece Desc = "desc"

instance ToHttpApiData Filter where
  toUrlPiece (FilterByName n) = n
  toUrlPiece (FilterByDate op d) = toUrlPiece op <> toUrlPiece d

instance ToHttpApiData FilterOp where
  toUrlPiece OpGT  = ">"
  toUrlPiece OpGTE = ">="
  toUrlPiece OpEQ  = "="
  toUrlPiece OpLTE = "<="
  toUrlPiece OpLT  = "<"

instance ToHttpApiData FilterDate where
  toUrlPiece = Text.pack . show
