module Tape
(Tape
,TWord
,Symbol
,getWord
,createTape)where

data Tape = Tape String String
type Symbol = Char
type TWord = String
createTape :: TWord -> Symbol -> Tape
createTape word emptySymb = Tape emptySymbs (word ++ emptySymbs)
    where
      emptySymbs = [emptySymb, emptySymb..]

getWord :: Tape -> Symbol -> TWord
getWord (Tape ls rs) s = (takeWhile (/=s) ls ) ++ (takeWhile (/=s) rs)
