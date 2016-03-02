module TuringCommandModule where

import TuringMachineModule
import Data.Array

data TuringFunction = Function
    { runFunction :: TMachine -> TMachine }

type TC_Index = (TM_State, TM_Char)
type TCommand = (TC_Index, TuringFunction)
--type TCommandList = Array TC_Index TuringFunction
type TCommandList = [TCommand]

buildCommands :: [TCommand] -> [TCommand]--Array TC_Index TuringFunction
buildCommands ts = ts--array ((0,lowestTM_Char),(maxInt,highestTM_Char)) ts

getCommand :: [TCommand] -> TC_Index -> TuringFunction
getCommand [] i         = error "TuringCommandModule.getCommand: No matching command."
getCommand (t:ts) i
    | fst t == i                = snd t
    | fst t == (fst i, dummy)   = snd t --correct to account for state.
    | otherwise                 = getCommand ts i

buildTCommand :: [String] -> TCommand
buildTCommand (a:b:c:d:e:[]) = (index, function)
    where
        index = (checkState, checkCurrent)
        checkState = (read a :: Int)
        checkCurrent = head b
        function = buildFunction c d e

buildFunction :: String -> String -> String -> TuringFunction
buildFunction newVal direction newState = Function $ \tm -> alterMachine nV nD nS tm
    where
        nV = head newVal
        nD = case head direction of
                '>' -> RightDir
                '<' -> LeftDir
                '-' -> StayDir
        nS = (read newState :: Int)
