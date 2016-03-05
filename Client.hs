{-# LANGUAGE RecordWildCards #-}

module Client
( Client
, make
, applyLocal
, applyRemote
, checkSend
)
where

import Patch (Patch)
import qualified Patch

import Data.Text (Text)

data Client = Client
  { document :: Text
  , revision :: !Int
  , inflight :: Patch
  , buffered :: Patch
  }
  deriving (Show)

make :: Text -> Int -> Client
make document revision = Client{..}
  where
  inflight = Patch.empty
  buffered = Patch.empty

applyLocal :: Client -> Patch -> Client
applyLocal client patch = client
  { buffered = Patch.append (buffered client) patch
  }

applyRemote :: Client -> Patch -> (Client, Patch)
applyRemote client patch
  | Patch.apply (inflight client) (document client)
    == Patch.apply patch (document client)
  = (client
    { document = Patch.apply patch (document client)
    , revision = revision client + 1
    , inflight = Patch.empty
    }, Patch.empty)
applyRemote client patch = (client
  { document = Patch.apply patch (document client)
  , revision = revision client + 1
  , inflight = inflight'
  , buffered = buffered'
  }, patch'')
  where
  (patch', inflight') = Patch.transform patch (inflight client)
  (patch'', buffered') = Patch.transform patch' (buffered client)

checkSend :: Client -> Maybe (Client, Patch, Int)
checkSend client =
  if emptyInflate && not emptyBuffered
    then let client' = client
               { inflight = buffered client
               , buffered = Patch.empty
               }
         in Just (client', inflight client', revision client')
    else Nothing
  where
  emptyInflate =
    Patch.apply (inflight client) (document client) == document client
  emptyBuffered =
    Patch.apply (buffered client) (document client) == document client
