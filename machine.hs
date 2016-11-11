import Data.Map as Map
import Data.List as List

data Tape = Tape String String

data Dir = LeftSh | RightSh | Stay

type Alphabet = Char
type State = Int
type Shift = Dir

shift :: Tape -> Dir -> Tape
shift tape Stay = tape
shift (Tape leftPart (r : rs)) RightSh = Tape (r : leftPart) (rs)
shift (Tape (l : ls) rightPart) LeftSh = Tape ls (l : rightPart)

getSymb :: Tape -> Char
getSymb (Tape leftPart (r : rs)) = r

writeSymb :: Tape -> Char -> Tape
writeSymb (Tape leftPart (r : rs)) symb = Tape leftPart (symb : rs)


turingMachine :: Map.Map (Alphabet, State) (Alphabet, State, Shift) -> (Tape, State) -> Tape
turingMachine program (tape,state) = fst $ last $ List.unfoldr foldFunc (tape, state)
    where
          foldFunc (tape, state) =
              let newTape = shift (writeSymb tape newSymb) dir
                  (newSymb, newState, dir) = case Map.lookup (getSymb tape, state) program of
                          Just (n,st,sh) -> (n, st, sh)
                          Nothing -> (' ',0,Stay)
              in if state == 0 then Nothing
                 else Just ((tape, state), (newTape, newState))
