import Data.Map as Map
import Data.List as List
import Tape

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
