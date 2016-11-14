module Tape
(Tape
,Dir(..)
,State
,getSymb
,writeSymb
,shift
,getWord
,createTape)where

data Dir = LeftSh | RightSh | Stay
     deriving (Read)
data Tape = Tape String String Char
type State = Int

createTape :: String -> Char -> Tape
createTape word blank = Tape bs (word ++ bs) blank
    where
      bs = repeat blank

getWord :: Tape -> String
getWord (Tape ls rs b) = reverse (takeWhile (/=b) ls ) ++ (takeWhile (/=b) rs)

shift :: Tape -> Dir -> Tape
shift tape Stay = tape
shift (Tape ls (r : rs) b) RightSh = Tape (r : ls) rs b
shift (Tape (l : ls) rs b) LeftSh = Tape ls (l : rs) b

getSymb :: Tape -> Char
getSymb (Tape _ (r : _) _) = r

writeSymb :: Tape -> Char -> Tape
writeSymb (Tape ls (_ : rs) b) symb = Tape ls (symb : rs) b
