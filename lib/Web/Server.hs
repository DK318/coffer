
module Web.Server where

import Control.Monad.Except
import Control.Monad.Catch (try, SomeException)

import Data.Function ((&))
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.List.NonEmpty (NonEmpty)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)

import Polysemy
import Polysemy.Error hiding (try)

import System.Exit (die)

import Web.API
import CLI.Types
import Entry
import Error
import Directory
import Backend
import Backend.Vault.Kv

import Servant.API
import Servant.Server
import Servant.Client

url :: BaseUrl
url = BaseUrl Http "127.0.0.1" 8200 ""

mount :: Text
mount = "secret"

runVaultIO' :: VaultToken -> Sem '[BackendEffect, Error CofferError, Embed IO, Final IO ] a -> IO (Either CofferError a)
runVaultIO' token action =
  runVaultIO url token mount action
    & errorToIOFinal @CofferError
    & embedToFinal @IO
    & runFinal

reportErrors :: IO (Either CofferError a) -> Handler a
reportErrors io = do
  eea <- liftIO do
    try @_ @SomeException io

  case eea of
    Left ioError -> do
      throwError err500 { errBody = errorToServantErrorMsg ioError }

    Right (Left (Vault e@EntryNotFound {})) -> do
      throwError err404 { errBody = errorToServantErrorMsg e }

    Right (Left e) -> do
      throwError err500 { errBody = errorToServantErrorMsg e }

    Right (Right a) -> do
      return a

errorToServantErrorMsg :: Show a => a -> Bytes.Lazy.ByteString
errorToServantErrorMsg
  = Bytes.Lazy.fromStrict
  . Text.encodeUtf8
  . Text.pack
  . show

makeServer
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> Server API
makeServer run token
  =    view   run token
  :<|> create run token
  :<|>
    (\txt ->
         private run token txt
    :<|> public  run token txt
    :<|> set     run token txt
    )
  :<|> deleteField run token
  :<|> find'       run token
  :<|> rename      run token
  :<|> copy'       run token
  :<|> delete'     run token
  :<|> tag         run token

view
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Maybe FieldKey
  -> Handler (Maybe (Directory (Entry, NonEmpty (FieldKey, Field))))
view run token voPath voFieldName = run token $ CmdView ViewOptions {voPath, voFieldName}

create
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Bool
  -> [Text]
  -> (HashMap FieldKey Text, HashMap FieldKey Text)
  -> Handler CreateResult
create run token coPath coForce coTags (fields, privateFields) =
  run token $ CmdCreate CreateOptions
    { coPath
    , coEdit = False
    , coForce
    , coTags
    , coFields        = fields        & HashMap.toList & map (uncurry FieldInfo)
    , coPrivateFields = privateFields & HashMap.toList & map (uncurry FieldInfo)
    }

private
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Handler SetFieldResult
private run token sfoPath =
  run token $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName = FieldKey ""
    , sfoFieldContents = ""
    , sfoVisibility = Just Private
    }

public
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Handler SetFieldResult
public run token sfoPath =
  run token $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName = FieldKey ""
    , sfoFieldContents = ""
    , sfoVisibility = Just Public
    }

set
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> FieldKey
  -> Text
  -> Handler SetFieldResult
set run token sfoPath sfoFieldName sfoFieldContents =
  run token $ CmdSetField SetFieldOptions
    { sfoPath
    , sfoFieldName
    , sfoFieldContents
    , sfoVisibility = Nothing
    }

deleteField
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> FieldKey
  -> Handler DeleteFieldResult
deleteField run token dfoPath dfoFieldName =
  run token $ CmdDeleteField DeleteFieldOptions
    { dfoPath
    , dfoFieldName
    }

find'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Maybe Path
  -> Maybe Text
  -> Maybe (Sort, Direction)
  -> [Filter]
  -> [(FieldKey, FilterField)]
  -> Handler (Maybe (Directory Entry))
find' run token foPath foText foSort foFilters foFilterFields =
  run token $ CmdFind FindOptions
    { foPath
    , foText
    , foSort
    , foFilters
    , foFilterFields
    }

rename
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Path
  -> Bool
  -> Handler RenameResult
rename run token roOldPath roNewPath roForce =
  run token $ CmdRename RenameOptions
    { roOldPath
    , roNewPath
    , roForce
    }

copy'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Path
  -> Bool
  -> Handler CopyResult
copy' run token cpoOldPath cpoNewPath cpoForce =
  run token $ CmdCopy CopyOptions
    { cpoNewPath
    , cpoOldPath
    , cpoForce
    }

delete'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Bool
  -> Handler DeleteResult
delete' run token doPath doRecursive =
  run token $ CmdDelete DeleteOptions
    { doPath
    , doRecursive
    }

tag
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Path
  -> Text
  -> Bool
  -> Handler TagResult
tag run token toPath toTagName toDelete =
  run token $ CmdTag TagOptions
    { toPath
    , toTagName
    , toDelete
    }
