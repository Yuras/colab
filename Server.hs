{-# LANGUAGE RecordWildCards #-}

module Server
( Server
, make
, apply
)
where

import Patch (Patch)
import qualified Patch

import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

data Server = Server
  { document :: Text
  , initialRevision :: Int
  , patches :: Seq Patch
  }
  deriving (Show)

make :: Text -> Int -> Server
make document initialRevision = Server{..}
  where
  patches = mempty

apply :: Server -> Patch -> Int -> (Server, Patch)
apply server patch revision = (server', patch')
  where
  server' = server
    { patches = patches server Seq.|> patch'
    , document = Patch.apply patch' (document server)
    }
  (_, patch') = Patch.transform delta patch
  delta = Foldable.foldl' Patch.append Patch.empty deltaPatches
  deltaPatches = Seq.drop (revision - initialRevision server) (patches server)
