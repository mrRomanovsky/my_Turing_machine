import Data.Map as Map
import Data.List as List

type Tape = (String, String)

data Dir = Left | Right | Stay

shift :: Tape -> Dir -> Tape

getSymb :: Tape -> Char


writeSymb :: Tape -> Char -> Tape


turingMachine :: Map.Map (Alphabet, State) (Alphabet, State, Shift) -> (Tape, Position, State) -> Tape
turinaMachine program tape state = snd $ last $ List.unfoldr (\ ( tape, state ) -> if state == 0 then Nothing else Just ((tape, state), (newTape, newState)))
    where newTape = shift (writeSymb tape newSymb) dir
          (newSymb, newState, dir) = case Map.lookup (getSymb tape, state) program of
          Just (n,st,sh) -> (n, st, sh)
          Nothing -> (' ',0,Stay)
