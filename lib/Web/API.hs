{-# options_ghc -Wno-orphans #-}

module Web.API where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import qualified Data.Text as Text

import Servant.API

import CLI.Types
import Entry

type API
  = "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParam "path"  Text
      :> OptionalParam "field" FieldKey
      :> Get '[JSON] Value

    :<|> "create"
      :> RequiredParam  "path" Text
      :> QueryFlag      "force"
      :> OptionalParams "tag"   Text
      :> ReqBody '[JSON] (HashMap FieldKey Text, HashMap FieldKey Text)
      :> PostNoContent

    :<|> "set-field"
      :> RequiredParam "path" Text
      :>
        (    "private" :> PostNoContent
        :<|> "public"  :> PostNoContent
        :<|> RequiredParam "field" FieldKey
          :> ReqBody '[JSON] Text
          :> PostNoContent
        )

    :<|> "delete-field"
      :> RequiredParam "path" Text
      :> RequiredParam "field" FieldKey
      :> DeleteNoContent

    {-
      { foPath :: Maybe Text
  , foText :: Maybe Text
  , foSort :: Maybe Sort
  , foSortField :: Maybe SortField
  , foFilters :: [Filter]
  , foFilterFields :: [(FieldKey, FilterField)]

    -}
    :<|> "find"
      :> OptionalParam  "path" Text
      :> OptionalParam  "text" Text
      :> OptionalParam  "sort" Sort
      :> OptionalParam  "sort-field" SortField
      :> OptionalParams "filter" Filter
      :> OptionalParams "filter-field" (FieldKey, FilterField)
      :> Get '[JSON] Value

    :<|> "rename"
      :> RequiredParam "old-path" Text
      :> RequiredParam "new-path" Text
      :> QueryFlag     "force"
      :> PostNoContent

    :<|> "copy"
      :> RequiredParam "old-path" Text
      :> RequiredParam "new-path" Text
      :> QueryFlag     "force"
      :> PostNoContent

    :<|> "delete"
      :> RequiredParam "path" Text
      :> QueryFlag     "recursive"
      :> PostNoContent

    :<|> "tag"
      :> RequiredParam "path" Text
      :> RequiredParam "tag" Text
      :> QueryFlag     "delete"
      :> PostNoContent
    )

type RequiredParam  = QueryParam' [Required, Strict]
type OptionalParam  = QueryParam
type OptionalParams = QueryParams

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
