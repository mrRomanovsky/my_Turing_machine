import Data.Map as Map
import Data.List as List
import Tape

data Dir = LeftSh | RightSh | Stay
type State = Int
type Shift = Dir

shift :: Tape -> Dir -> Tape
shift tape Stay = tape
shift (Tape ls (r : rs)) RightSh = Tape (r : ls) rs
shift (Tape (l : ls) rs) LeftSh = Tape ls (l : rs)

getSymb :: Tape -> Char
getSymb (Tape _ (r : _)) = r

writeSymb :: Tape -> Char -> Tape
writeSymb (Tape ls (_ : rs)) symb = Tape ls (symb : rs)

turingMachine :: Map.Map (Char, State) (Char, State, Shift) -> (Tape, State) -> Tape
turingMachine program (tape,state) = fst $ last $ List.unfoldr foldFunc (tape, state)
    where
          foldFunc (tape, state) =
              let newTape = shift (writeSymb tape newSymb) dir
                  (newSymb, newState, dir) = case Map.lookup (getSymb tape, state) program of
                          Just (n,st,sh) -> (n, st, sh)
                          Nothing -> (' ',0,Stay)
              in if state == 0 then Nothing
                 else Just ((tape, state), (newTape, newState))
