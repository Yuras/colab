
module Edit
( Edit (..)
, apply
, transform
)
where

import qualified Data.Tuple as Tuple
import Data.Text (Text)
import qualified Data.Text as Text

data Edit
  = Insert Int Text
  | Delete Int Int
  deriving (Show)

{- Note Edit properties

let (a', b') = transform a b
in (apply a' . apply b) doc == (apply b' . apply a) doc

-}

apply :: Edit -> Text -> Text
apply (Delete from count) txt =
  let (lt, rest) = Text.splitAt from txt
      rt = Text.drop count rest
  in Text.append lt rt
apply (Insert at t) txt =
  let (lt, rt) = Text.splitAt at txt
  in Text.concat [lt, t, rt]

transform :: Edit -> Edit -> (Edit, Edit)

transform (Insert at1 t1) (Insert at2 t2) =
  if at1 > at2
    then (Insert (at1 + Text.length t2) t1, Insert at2 t2)
    else (Insert at1 t1, Insert (at2 + Text.length t1) t2)

transform (Delete from1 count1) (Delete from2 count2)
  | from2 >= from1 + count1
  = (Delete from1 count1, Delete (from2 - count1) count2)
  | from1 >= from2 + count2
  = (Delete (from1 - count2) count1, Delete from2 count2)
  | from1 >= from2 && from1 + count1 <= from2 + count2
  = (Delete from2 0, Delete from2 (count2 - count1))
  | from2 >= from1 && from2 + count2 <= from1 + count1
  = (Delete from1 (count1 - count2), Delete from1 0)
  | from1 >= from2
  = let d = from2 + count2 - from1
    in (Delete from2 (count1 - d), Delete from2 (count2 - d))
  | otherwise
  = let d = from1 + count1 - from2
    in (Delete from1 (count1 - d), Delete from1 (count2 - d))

transform (Insert at t) (Delete from count)
  | at >= from && at < from + count
  = (Insert from Text.empty, Delete from (count + Text.length t))
  | at < from
  = (Insert at t, Delete (from + Text.length t) count)
  | otherwise
  = (Insert (at - count) t, Delete from count)

transform d@Delete{} i@Insert{} =
  Tuple.swap $ transform i d
