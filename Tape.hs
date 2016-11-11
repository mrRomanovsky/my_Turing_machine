module Tape
(Tape
,getWord
,createTape)where

data Tape = Tape String String
createTape :: String -> Char -> Tape
createTape word emptySymb = Tape bs (word ++ bs)
    where
      bs = [emptySymb, emptySymb..]

getWord :: Tape -> Char -> String
getWord (Tape ls rs) s = (takeWhile (/=s) ls ) ++ (takeWhile (/=s) rs)
