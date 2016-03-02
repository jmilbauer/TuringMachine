module Main where

import TuringConstructModule
import TuringCommandModule
import TuringMachineModule
import Data.Array
import Control.Concurrent
import Control.Monad

tm = buildMachine ["0","0001","1","100E"]
tc = buildCommands $ map buildTCommand  [ ["1", "1", "1", ">", "1"]
                                        , ["1", "0", "0", ">", "1"]
                                        , ["1", "E", "E", "<", "-1"]
                                        ]
tconstruct = buildConstruct tm tc

getTMachineLines :: String -> [String]
getTMachineLines s = do
    let input = lines s
    let output = map cleanup input
    removeEmpties output

removeEmpties :: [String] -> [String]
removeEmpties [] = []
removeEmpties (x:xs)
    | x == ""   = removeEmpties xs
    | otherwise = x : removeEmpties xs

--turns comment strings and newlines into empty strings
cleanup :: String -> String
cleanup str@(s:ss)
    | s == '#'      = ""
    | otherwise     = str
cleanup x           = x

printN :: String -> IO ()
printN str = do
    replicateM_ 1 (putStrLn str)

main :: IO ()
main = do
    x <- getContents
    let input = getTMachineLines x

    let initializer = "0" : (words (head input))
    let commandizers = map words $ tail input ++ ["-1 * * - -2"]

    let commands = buildCommands $ map buildTCommand commandizers
    putStrLn $ "test"
    let machine = buildMachine initializer

    let construct = buildConstruct machine commands
    let history = buildHistory construct

    mapM_ printN history
