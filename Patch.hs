
module Patch
( Patch (..)
, empty
, singleton
, append
, apply
, transform
)
where

import Edit (Edit)
import qualified Edit

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype Patch = Patch
  { edits :: Vector Edit
  }
  deriving (Show)

{- Note Patch properties

let (a', b') = transform (a, b)
in apply (a `append` b') doc == apply (b `append` a') doc

-}

instance Monoid Patch where
  mempty = empty
  mappend = append

empty :: Patch
empty = Patch Vector.empty

singleton :: Edit -> Patch
singleton edit = Patch (Vector.singleton edit)

append :: Patch -> Patch -> Patch
append p1 p2 = Patch (edits p1 Vector.++ edits p2)

apply :: Patch -> Text -> Text
apply (Patch p) txt = Vector.foldl' step txt p
  where
  step txt' edit = Edit.apply edit txt'

transform :: Patch -> Patch -> (Patch, Patch)
transform (Patch a) b =
  let step (a', b') a1 =
        let (a1', b'') = transformEdit a1 b'
        in (append a' (singleton a1'), b'')
  in Vector.foldl' step (empty, b) a

transformEdit :: Edit -> Patch -> (Edit, Patch)
transformEdit a (Patch b) =
  let step (a', b') b1 =
        let (a'', b1') = Edit.transform a' b1
        in (a'', append b' (singleton b1'))
  in Vector.foldl' step (a, empty) b
