{-# options_ghc -Wno-orphans #-}

module Web.API where

import Control.Monad (guard, void)

import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.HashMap.Strict
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Void (Void)
import Data.Time (LocalTime (LocalTime), makeTimeOfDayValid)
import Data.Time.Calendar.Compat ( fromGregorianValid )
import Data.Time.Calendar.Month.Compat (fromYearMonthValid)
import Data.Fixed (Pico)

import GHC.Generics (Generic)

import Servant.API

import Text.Megaparsec

import CLI.Types
import Entry
import Backend.Vault.Kv
import Directory (Directory, Path, mkPath)

type API
  = Header' [Required, Strict] "token" VaultToken
  :> "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParam "path"  Path
      :> OptionalParam "field" FieldKey
      :> Get '[JSON] (Maybe (Directory (Entry, NonEmpty (FieldKey, Field))))

    :<|> "create"
      :> RequiredParam  "path" Path
      :> QueryFlag      "force"
      :> OptionalParams "tag"   Text
      :> ReqBody '[JSON] (HashMap FieldKey Text, HashMap FieldKey Text)
      :> Post '[JSON] CreateResult

    :<|> "set-field"
      :> RequiredParam "path" Path
      :>
        (    "private" :> Post '[JSON] SetFieldResult
        :<|> "public"  :> Post '[JSON] SetFieldResult
        :<|> RequiredParam "field" FieldKey
          :> ReqBody '[JSON] Text
          :> Post '[JSON] SetFieldResult
        )

    :<|> "delete-field"
      :> RequiredParam "path" Path
      :> RequiredParam "field" FieldKey
      :> Delete '[JSON] DeleteFieldResult

    :<|> "find"
      :> OptionalParam  "path" Path
      :> OptionalParam  "text" Text
      :> OptionalParam  "sort-field" (Sort, Direction)
      :> OptionalParams "filter" Filter
      :> OptionalParams "filter-field" (FieldKey, FilterField)
      :> Get '[JSON] (Maybe (Directory Entry))

    :<|> "rename"
      :> RequiredParam "old-path" Path
      :> RequiredParam "new-path" Path
      :> QueryFlag     "force"
      :> Post '[JSON] RenameResult

    :<|> "copy"
      :> RequiredParam "old-path" Path
      :> RequiredParam "new-path" Path
      :> QueryFlag     "force"
      :> Post '[JSON] CopyResult

    :<|> "delete"
      :> RequiredParam "path" Path
      :> QueryFlag     "recursive"
      :> Post '[JSON] DeleteResult

    :<|> "tag"
      :> RequiredParam "path" Path
      :> RequiredParam "tag" Text
      :> QueryFlag     "delete"
      :> Post '[JSON] TagResult
    )

newtype Token = Token { getToken :: Text }
  deriving stock (Eq, Ord, Show, Generic)

type RequiredParam  = QueryParam' [Required, Strict]
type OptionalParam  = QueryParam
type OptionalParams = QueryParams

deriving newtype instance ToHttpApiData FieldKey

instance FromHttpApiData Filter where
  parseUrlPiece = toServantParser do
    choice
      [ do
          void "name~"
          name <- takeRest
          guard $ not $ Text.null name
          return $ FilterByName name
      , do
          void "date"
          op <- choice
            [ ">=" $> OpGTE
            , "<=" $> OpLTE
            , ">"  $> OpGT
            , "<"  $> OpLT
            , "="  $> OpEQ
            ]
          date <- parseDate
          return (FilterByDate op date)
      ]

instance FromHttpApiData (Sort, Direction) where
  parseUrlPiece = toServantParser do
    choice
      [ try do
          means <- choice [SortByEntryName <$ "name", SortByEntryDate <$ "date"]
          void ":"
          direction <- choice [Asc <$ "asc", Desc <$ "desc"]
          eof
          return (means, direction)

      , do
          (field, _) <- match $ some $ noneOf [':']
          case newFieldKey field of
            Nothing -> fail "field name is incorrect"
            Just field -> do
              void ":"
              means <- choice
                [ SortByFieldValue field <$ "value"
                , SortByFieldDate  field <$ "date"
                ]
              void ":"
              direction <- choice [Asc <$ "asc", Desc <$ "desc"]
              eof
              return (means, direction)
      ]

instance FromHttpApiData (FieldKey, FilterField) where
  parseUrlPiece = toServantParser do
    field <- fieldName
    void ":"
    case newFieldKey field of
      Nothing -> fail "field name is incorrect"
      Just field -> do
        choice
          [ do
              void "value~"
              value <- takeRest
              guard $ not $ Text.null value
              return (field, FilterFieldByValue value)
          , do
              void "date"
              op <- choice
                [ ">=" $> OpGTE
                , "<=" $> OpLTE
                , ">"  $> OpGT
                , "<"  $> OpLT
                , "="  $> OpEQ
                ]
              date <- parseDate
              return (field, FilterFieldByDate op date)
          ]

parseDate :: Parsec Void Text FilterDate
parseDate = do
  y <- read <$> times 4 digit
  do  void "-"
      m <- read <$> times 2 digit
      do  void "-"
          d <- read <$> times 2 digit
          day <- maybe (fail "invalid year/month/date") return do
            fromGregorianValid y m d
          do  void "#"
              h <- read <$> times 2 digit
              void ":"
              mm <- read <$> times 2 digit
              void ":"
              s <- read <$> times 2 digit
              local <- maybe (fail "invalid hour/minute/second") return do
                makeTimeOfDayValid h mm (fromIntegral @Int @Pico s)
              return $ FDTime (LocalTime day local)
            <|> do
              maybe (fail "invalid year/month/date") (return . FDDay)
                $ fromGregorianValid y m d
        <|> do
          maybe (fail "invalid year/month") (return . FDMonth)
            $ fromYearMonthValid y m
    <|> do
      return (FDYear y)

digit :: Parsec Void Text Char
digit = oneOf ['0'..'9'] <?> "digit"

times :: Applicative m => Int -> m a -> m [a]
times = (sequenceA .) . replicate

fieldName :: Parsec Void Text Text
fieldName = fst <$> match (some $ noneOf [':'])

deriving newtype instance FromHttpApiData VaultToken
deriving newtype instance FromHttpApiData FieldKey

instance FromHttpApiData Path where
  parseUrlPiece = toServantParser do
    mkPath <$> takeRest

toServantParser :: Parsec Void Text a -> Text -> Either Text a
toServantParser p = bimap (Text.pack . errorBundlePretty) id . parse p "<url>"

-- instance ToHttpApiData Sort where
--   toUrlPiece =
--       = SortByEntryName
--   | SortByEntryDate
--   | SortByFieldValue FieldKey
--   | SortByFieldDate FieldKey

-- instance ToHttpApiData SortMeans where
--   toUrlPiece SMDate = "date"
--   toUrlPiece SMName = "name"

-- instance ToHttpApiData Direction where
--   toUrlPiece Asc  = "asc"
--   toUrlPiece Desc = "desc"

-- instance ToHttpApiData Filter where
--   toUrlPiece (FilterByName n) = n
--   toUrlPiece (FilterByDate op d) = toUrlPiece op <> toUrlPiece d

-- instance ToHttpApiData FilterOp where
--   toUrlPiece OpGT  = ">"
--   toUrlPiece OpGTE = ">="
--   toUrlPiece OpEQ  = "="
--   toUrlPiece OpLTE = "<="
--   toUrlPiece OpLT  = "<"

-- instance ToHttpApiData FilterDate where
--   toUrlPiece = Text.pack . show
