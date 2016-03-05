{-# LANGUAGE OverloadedStrings #-}

module Proto
( encodePatchAndRev
, encodePatch
, patchAndRevParser
, patchParser
)
where

import Patch (Patch)
import qualified Patch
import qualified Edit

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

encodePatchAndRev :: Patch -> Int -> Lazy.ByteString
encodePatchAndRev patch revision = Aeson.encode $ Aeson.object
  [ "patch" Aeson..= encodePatch patch
  , "revision" Aeson..= revision
  ]

encodePatch :: Patch -> Aeson.Value
encodePatch patch = Aeson.Array $ Vector.map encodeEdit (Patch.edits patch)
  where
  encodeEdit (Edit.Insert at txt) = Aeson.object
    [ "type" Aeson..= Aeson.String "insert"
    , "at" Aeson..= at
    , "text" Aeson..= txt
    ]
  encodeEdit (Edit.Delete from count) = Aeson.object
    [ "type" Aeson..= Aeson.String "delete"
    , "from" Aeson..= from
    , "count" Aeson..= count
    ]

patchAndRevParser :: Aeson.Value -> Aeson.Parser (Patch, Int)
patchAndRevParser = Aeson.withObject "patch and revision" $ \o -> do
  patch <- o Aeson..: "patch" >>= patchParser
  revision <- o Aeson..: "revision"
  return (patch, revision)

patchParser :: Aeson.Value -> Aeson.Parser Patch
patchParser = Aeson.withArray "patch" $ \arr -> do
  edits <- Vector.mapM editParser arr
  return (Patch.Patch edits)
  where
  editParser = Aeson.withObject "edit" $ \o -> do
    tp <- o Aeson..: "type"
    case (tp :: Text) of
      "insert" -> do
        at <- o Aeson..: "at"
        text <- o Aeson..: "text"
        return (Edit.Insert at text)
      "delete" -> do
        from <- o Aeson..: "from"
        count <- o Aeson..: "count"
        return (Edit.Delete from count)
      _ -> fail "unknow edit type"
