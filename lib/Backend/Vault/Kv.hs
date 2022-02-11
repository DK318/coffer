{-# LANGUAGE TemplateHaskell #-}

module Backend.Vault.Kv
  ( VaultKvBackend
  , runVaultIO
  ) where

import qualified Entry                        as E
import qualified Backend.Vault.Kv.Internal    as I

import           Error                        (CofferError (..))
import           Backend                      (Backend (..), BackendEffect (..))

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Data.Text.Lazy               as TL
import qualified Data.HashMap.Internal.Strict as HS
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Text              as A
import qualified Data.Scientific              as S
import qualified Toml

import           Control.Monad                (void)
import           Servant.Client               (BaseUrl (BaseUrl), Scheme (Https, Http), mkClientEnv, runClientM, ClientError (..), parseBaseUrl, showBaseUrl)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import           Polysemy.Error               (Error, throw, runError)
import           Control.Exception.Lens       (exception)
import           Control.Monad.State          (evalState)
import           Toml                         (TomlCodec)
import           GHC.Generics                 (Generic)
import           Control.Applicative          (Alternative(empty))
import           Validation                   (Validation(Success, Failure))
import           Data.Time.Format.ISO8601     (iso8601ParseM, iso8601Show)
import           Data.Time                    (UTCTime)
import           Control.Exception            (catch)
import           Servant.Client.Core.Response (responseStatusCode)
import           Servant.Client.Core.Request  (RequestF (Request))
import           Network.HTTP.Types           (statusCode)

import           Polysemy
import           Control.Lens
import Coffer.Path (entryPathSegments, pathSegments)

data VaultKvBackend =
  VaultKvBackend
  { _vbName :: T.Text
  , _vbAddress :: BaseUrl
  , _vbMount :: T.Text
  , _vbToken :: I.VaultToken
  }
  deriving (Show)

{-# INLINE didimatch #-}
didimatch
    :: (b -> Maybe a)  -- ^ Mapper for consumer
    -> (a -> Maybe b)     -- ^ Mapper for producer
    -> TomlCodec a  -- ^ Source 'Codec' object
    -> TomlCodec b  -- ^ Target 'Codec' object
didimatch matchB matchA codec = Toml.Codec
    { Toml.codecRead = \t -> case Toml.codecRead codec t of
        Success a -> maybe empty Success (matchA a)
        Failure b -> Failure b
    , Toml.codecWrite = \c -> case matchB c of
        Nothing -> empty
        Just d  -> Toml.codecWrite codec d >>= maybe empty pure . matchA
    }


vaultKvCodec :: TomlCodec VaultKvBackend
vaultKvCodec = VaultKvBackend
                  <$> Toml.text "name" Toml..= _vbName
                  <*> didimatch baseUrlToText textToBaseUrl (Toml.text "address") Toml..= _vbAddress
                  <*> Toml.text "mount" Toml..= _vbMount
                  <*> didimatch tokenToText textToToken (Toml.text "token") Toml..= _vbToken
  where tokenToText (I.VaultToken t) = Just t
        textToToken t = Just $ I.VaultToken t
        baseUrlToText = Just . T.pack . showBaseUrl
        textToBaseUrl = parseBaseUrl . T.unpack

data FieldMetadata = FieldMetadata
  { _dateModified :: UTCTime
  , _private :: Bool
  }
  deriving (Show, Generic)
makeLenses ''FieldMetadata

instance A.ToJSON FieldMetadata where
instance A.FromJSON FieldMetadata where

data CofferSpecials =
  CofferSpecials
  { _masterKey :: Maybe T.Text
  , _globalDateModified :: UTCTime
  , _fields :: HS.HashMap T.Text FieldMetadata
  , _tags :: [T.Text]
  }
  deriving (Show, Generic)
makeLenses ''CofferSpecials

instance A.ToJSON CofferSpecials where
instance A.FromJSON CofferSpecials where

runVaultIO :: Member (Embed IO) r
           => Member (Error CofferError) r
           => BaseUrl
           -> I.VaultToken
           -> T.Text
           -> Sem (BackendEffect ': r) a
           -> Sem r a
runVaultIO url token mount = interpret $
  \x -> do
    env <-
      case url of
        (BaseUrl Http _ _ _) -> do
          manager <- embed $ newManager defaultManagerSettings
          pure $ mkClientEnv manager url
        (BaseUrl Https _ _ _) -> do
          manager <- embed $ newManager tlsManagerSettings
          pure $ mkClientEnv manager url

    case x of
      WriteSecret entry -> do
        let cofferSpecials = CofferSpecials
                             { _masterKey =
                               entry ^. E.masterField <&> E.getFieldKey
                             , _globalDateModified = entry ^. E.dateModified
                             , _fields =
                               entry ^. E.fields & traverse %~
                                 (\f ->
                                 FieldMetadata
                                 { _dateModified = f ^. E.dateModified
                                 , _private = f ^. E.private
                                 })
                               & HS.mapKeys E.getFieldKey
                             , _tags = map E.getEntryTag $ entry ^. E.tags
                             }
        let secret = I.PostSecret
              { I._cas = Nothing
              , I._pdata =
                    HS.insert "#$coffer" (TL.toStrict . TL.decodeUtf8 $ A.encode cofferSpecials)
                  . HS.map (^. E.value)
                  . HS.mapKeys E.getFieldKey
                  $ entry ^. E.fields
              }

        response <- embedCatch (entry ^. E.path . to entryPathSegments) do
          postSecret env (entry ^. E.path . to entryPathSegments) secret

        case response ^. I.ddata . at ("version" :: T.Text) of
          Just (A.Number i) -> maybe (throw MarshallingFailed) pure (S.toBoundedInteger i)
          _ -> throw MarshallingFailed
      ReadSecret path version ->
        runMaybe $ embedCatchMaybe (entryPathSegments path) (readSecret env (entryPathSegments path) version)
        >>= \(I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _) _ _ _) -> do
          cofferSpecials :: CofferSpecials <-
            maybeThrow (_data ^.at "#$coffer" >>= A.decodeStrict' . T.encodeUtf8)
          let secrets = HS.toList $ foldr HS.delete _data ["#$coffer"]
          let keyToField key = do
               _modTime <- cofferSpecials ^. fields.at key <&> (^. dateModified)
               _private <- cofferSpecials ^. fields.at key <&> (^. private)
               _value <- _data ^.at key
               _key <- E.newFieldKey key

               Just (_key
                    , E.newField _modTime _value
                      & E.private .~ _private
                    )

          fields <- maybeThrow $
            secrets & traverse %~ (keyToField . fst) & sequence <&> HS.fromList
          _tags <- maybeThrow $ cofferSpecials ^. tags & mapM E.newEntryTag

          pure $ E.newEntry path (cofferSpecials ^. globalDateModified)
            & E.masterField .~ (cofferSpecials ^. masterKey >>= E.newFieldKey)
            & E.fields .~ fields
            & E.tags .~ _tags
      ListSecrets path ->
        runMaybe (embedCatchMaybe (pathSegments path) (listSecrets env (pathSegments path)) <&> (^. I.ddata) <&> \(I.ListSecrets list) -> list)
      DeleteSecret path ->
        embedCatch (entryPathSegments path) (deleteSecret env (entryPathSegments path)) <&> const ()
  where postSecret env = (I.routes env ^. I.postSecret) mount token
        readSecret env = (I.routes env ^. I.readSecret) mount token
        listSecrets env = (I.routes env ^. I.listSecrets) mount token
        updateMetadata env = (I.routes env ^. I.updateMetadata) mount token
        deleteSecret env = (I.routes env ^. I.deleteSecret) mount token

        exceptionHandler :: [T.Text] -> ClientError -> Maybe CofferError
        exceptionHandler path =
          \case FailureResponse _request response ->
                    case statusCode $ responseStatusCode response of
                      404 -> Nothing
                      e -> Just $ OtherError (T.pack $ show e)
                DecodeFailure text response -> Just MarshallingFailed
                UnsupportedContentType mediaType response -> Just MarshallingFailed
                InvalidContentTypeHeader response -> Just MarshallingFailed
                ConnectionError excepton -> Just ConnectError
        embedCatch :: Member (Embed IO) r
                       => Member (Error CofferError) r
                       => [T.Text]
                       -> IO a
                       -> Sem r a
        embedCatch path io = embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler path)) >>=
          \case Left l -> pure l
                Right (Just r) -> throw r
                Right Nothing -> throw $ OtherError "404"

        embedCatchMaybe :: Member (Embed IO) r
                        => Member (Error CofferError) r
                        => Member (Error ()) r
                        => [T.Text]
                        -> IO a
                        -> Sem r a
        embedCatchMaybe path io = embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler path)) >>=
          \case Left l -> pure l
                Right (Just r) -> throw r
                Right Nothing -> throw ()

        runMaybe :: Sem (Error () ': r) t
                 -> Sem r (Maybe t)
        runMaybe x = runError x <&> \case
                       Left _ -> Nothing
                       Right a -> Just a

        maybeThrow :: Member (Error CofferError) r
                      => Maybe a
                      -> Sem r a
        maybeThrow = maybe (throw MarshallingFailed) pure

instance Backend VaultKvBackend where
  _name s = _vbName s
  _codecRead = Toml.codecRead vaultKvCodec
  _codecWrite a = Toml.codecWrite vaultKvCodec a
                  <* Toml.codecWrite (Toml.text "type") "vault"
  _runEffect b = runVaultIO (_vbAddress b) (_vbToken b) (_vbMount b)
