module TuringMachineModule where

data TMachine = Machine
    { tape :: TM_Tape
    , state :: TM_State
    }

data TM_Tape = Tape
    { prefix :: [TM_Char]
    , current :: TM_Char
    , suffix :: [TM_Char]
    }

type TM_Char = Char
type TM_State = Int
data TM_Dir = LeftDir | RightDir | StayDir
dummy :: TM_Char
dummy = '*'
blank :: TM_Char
blank = '_'

initialize :: TM_State -> TM_Tape -> TMachine
initialize s t = Machine { tape = t, state = s}

buildMachine :: [String] -> TMachine
buildMachine (a:b:c:d:[]) = initialize nS nT
    where
        nS = (read a :: Int)
        nT = Tape   { prefix = reverse b
                    , current = head c
                    , suffix = d }

-- // MOVING THE Tape

movetape :: TM_Dir -> TM_Tape -> TM_Tape
movetape dir tape = case dir of
    LeftDir -> left tape
    RightDir -> right tape
    StayDir -> stay tape

left :: TM_Tape -> TM_Tape
left t = case prefix t of
    [] -> Tape  { prefix = []
                , current = blank
                , suffix = current t : (suffix t)
                }
    (a:as) -> Tape  { prefix = as
                    , current = a
                    , suffix = current t : (suffix t)
                    }

right :: TM_Tape -> TM_Tape
right t = case suffix t of
    [] -> Tape  { prefix = current t : (prefix t)
                , current = blank
                , suffix = []
                }
    (a:as) -> Tape  { prefix = current t : (prefix t)
                    , current = a
                    , suffix = as
                    }

stay :: TM_Tape -> TM_Tape
stay = id


-- // CHANGING VALUES ON THE TAPE

alter :: TM_Char -> TM_Tape -> TM_Tape
alter ch tape
    | ch == dummy = tape
    | otherwise = Tape  { prefix = prefix tape
                        , current = ch
                        , suffix = suffix tape
                        }

changeValue :: TM_Char -> TMachine -> TMachine
changeValue ch tm = Machine { tape = alter ch (tape tm)
                            , state = state tm }

slideTape :: TM_Dir -> TMachine -> TMachine
slideTape dir tm = Machine  { tape = movetape dir (tape tm)
                            , state = state tm }

changeState :: TM_State -> TMachine -> TMachine
changeState newState tm = Machine   { tape = tape tm
                                    , state = newState }

alterMachine :: TM_Char -> TM_Dir -> TM_State -> TMachine -> TMachine
alterMachine nV nH nS tm = (changeState nS (slideTape nH (changeValue nV tm)))
