import Data.Map as Map
import Data.List as List
import Tape

turingMachine :: Map.Map (Char,State ) (Char, State, Dir) -> (Tape, State) -> Tape
turingMachine program (tape,state) = fst $ last $ List.unfoldr foldFunc (tape, state)
    where
          foldFunc (tape, state) =
              let newTape = shift (writeSymb tape newSymb) dir
                  (newSymb, newState, dir) = case Map.lookup (getSymb tape, state) program of
                          Just (n,st,sh) -> (n, st, sh)
                          Nothing -> (' ',0,Stay)
              in if state == 0 then Nothing
                 else Just ((tape, state), (newTape, newState))

tests :: Bool
tests = getWord (turingMachine inc (t, 1)) == "234"
        && getWord (turingMachine inc (t2, 1)) == "432342"
        && getWord (turingMachine bBeforeB (t3, 1)) == "bbbb"
        && getWord (turingMachine bBeforeB (t4, 1)) == "abbabb"
    where
      inc =  Map.fromList [(('1', 1), ('2', 1, RightSh)), (('2', 1), ('3', 1, RightSh)), (('3', 1), ('4', 1, RightSh)), (('s', 1), ('s', 0, Stay))]
      bBeforeB = Map.fromList [(('a', 1), ('a', 1, RightSh)), (('s', 1), ('s', 0, Stay)), (('b', 1), ('b', 2, LeftSh)), (('a', 2), ('b', 2, RightSh)), (('b', 2), ('b', 1, RightSh))]
      t = Tape.createTape "123" 's'
      t2 = Tape.createTape "321231" 's'
      t3 = Tape.createTape "abab" 's'
      t4 = Tape.createTape "aabaab" 's'

mapFromStr :: [String] -> Map.Map (Char,State ) (Char, State, Dir)
mapFromStr = Map.fromList . strToMap
    where
        strToMap =  List.foldr (\x acc -> toPair (List.words x) : acc) []
        toPair (s : st : ns : nst : dr : xs) = ((head s, toState st) , (head ns, toState nst, toDir dr ))
        toState st = read st :: State
        toDir dr = read dr :: Dir
