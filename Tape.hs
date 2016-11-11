module Tape
(Tape
,Dir
,State
,Shift
,getWord
,createTape)where

data Dir = LeftSh | RightSh | Stay
data Tape = Tape String String
type State = Int
type Shift = Dir

createTape :: String -> Char -> Tape
createTape word emptySymb = Tape bs (word ++ bs)
    where
      bs = [emptySymb, emptySymb..]

getWord :: Tape -> Char -> String
getWord (Tape ls rs) s = (takeWhile (/=s) ls ) ++ (takeWhile (/=s) rs)

shift :: Tape -> Dir -> Tape
shift tape Stay = tape
shift (Tape ls (r : rs)) RightSh = Tape (r : ls) rs
shift (Tape (l : ls) rs) LeftSh = Tape ls (l : rs)

getSymb :: Tape -> Char
getSymb (Tape _ (r : _)) = r

writeSymb :: Tape -> Char -> Tape
writeSymb (Tape ls (_ : rs)) symb = Tape ls (symb : rs)
