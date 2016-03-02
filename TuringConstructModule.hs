module TuringConstructModule where

import TuringCommandModule
import TuringMachineModule
import Data.Array

data TuringConstruct = Construct
    { getMachine :: TMachine
    , getCommands :: TCommandList }

buildConstruct :: TMachine -> TCommandList -> TuringConstruct
buildConstruct tm tcs = Construct tm tcs

iter :: TuringConstruct -> TuringConstruct
iter tc = do
    let machine     = getMachine tc
    let commands    = getCommands tc
    let tmState     = state machine
    let tmCurrent   = current (tape machine)
    let command     = getCommand commands (tmState, tmCurrent)
    let newmachine  = runFunction command machine
    Construct newmachine commands

toString :: TuringConstruct -> String
toString tc = do
    let tm          = getMachine tc
    let front       = reverse . take 30 $ (prefix (tape tm)) ++ (repeat '_')
    --let front = reverse . prefix $ tape tm
    let back        = take 30 $ (suffix (tape tm)) ++ (repeat '_')
    --let back = suffix $ tape tm
    let headval     = current $ tape tm
    let tapeString  =  "..." ++ front ++ "|" ++ headval:[] ++ "|" ++ back ++ "..."
    let stateString = "\nState: " ++ (show $ state tm)
    tapeString ++ "          " ++ stateString

buildHistory :: TuringConstruct -> [String]
buildHistory tc
    | state (getMachine tc) == -2       = []
    | otherwise = do
        let statePrint = toString tc
        let remainingStatePrints = buildHistory (iter tc)
        statePrint : remainingStatePrints
