module Test.Util where

import Test.Tasty.HUnit
import qualified Text.Megaparsec as P
import Data.Text (Text)
import CLI.Parser
import Text.Megaparsec (errorBundlePretty)
import CLI.Types (FieldInfo(..))
import Entry (FieldKey(..), fieldKeyAllowedChars)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Coffer.Types (Path(..), PathSegment, mkPathSegment, pathSegmentAllowedCharacters, EntryPath (..))

----------------------------------------------------------------------------
-- HUnit helpers
----------------------------------------------------------------------------

parserShouldSucceed :: (HasCallStack, Show a, Eq a) => MParser a -> Text -> a -> IO ()
parserShouldSucceed p input expected =
  case P.parse p "" input of
    Right actual -> actual @?= expected
    Left err -> assertFailure $ unlines
      [ "Failed to parse input."
      , ""
      , errorBundlePretty err
      ]

parserShouldFail :: (HasCallStack, Show a) => MParser a -> Text -> String -> IO ()
parserShouldFail p input expectedErr =
  case P.parse p "" input of
    Right actual -> assertFailure $ unlines
      [ "Expected parser to fail, but it succeeded."
      , ""
      , show actual
      ]
    Left err -> errorBundlePretty err @?= expectedErr

----------------------------------------------------------------------------
-- Hedgehog helpers
----------------------------------------------------------------------------

hparserShouldSucceed :: (HasCallStack, Show a, Eq a, MonadTest m) => MParser a -> Text -> a -> m ()
hparserShouldSucceed p input expected =
  case P.parse p "" input of
    Right actual -> actual === expected
    Left err -> do
      annotate $ unlines
        [ "Failed to parse input."
        , ""
        , errorBundlePretty err
        ]
      failure

----------------------------------------------------------------------------
-- Hedgehog generators
----------------------------------------------------------------------------

genPath :: Gen Path
genPath = Path <$> Gen.list (Range.linear 0 3) genPathSegment

genEntryPath :: Gen EntryPath
genEntryPath = EntryPath <$> Gen.nonEmpty (Range.linear 1 3) genPathSegment

genPathSegment :: Gen PathSegment
genPathSegment = do
  text <- Gen.text (Range.linear 1 5) (Gen.element pathSegmentAllowedCharacters)
  case mkPathSegment text of
    Right seg -> pure seg
    Left _ -> Gen.discard

genFieldKey :: Gen FieldKey
genFieldKey =
  FieldKey <$> Gen.text (Range.linear 1 20) (Gen.element fieldKeyAllowedChars)

genFieldInfo :: Gen FieldInfo
genFieldInfo =
  FieldInfo
    <$> genFieldKey
    <*> Gen.text (Range.linear 0 20)
          (Gen.frequency
            [ (4, Gen.unicode)
            , (1, pure '\n')
            ]
          )
