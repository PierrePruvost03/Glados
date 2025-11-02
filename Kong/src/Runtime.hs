{-# LANGUAGE ScopedTypeVariables #-}
module Runtime
    ( compileAndExecute
    , printVMResult
    , writeToBinary
    ) where

import System.Exit (exitSuccess, exitWith, ExitCode(..))
import DataStruct.VM (baseState, VMState(VMState, stack))
import DataStruct.Bytecode.Value (Instr, Value(VNumber))
import DataStruct.Bytecode.Number (Number(VInt))
import DataStruct.Ast (Ast)
import Compiler.BytecodeGen.Program.Program (compileProgram)
import VM.Execution (exec)
import ErrorPrinter (printCompileError)
import Data.Binary (encodeFile)
import Control.Exception
import System.IO

-- | Compile AST to bytecode and execute it
compileAndExecute :: [(String, [Ast])] -> IO ()
compileAndExecute fileAsts =
    either printCompileError executeInstructions $ compileProgram fileAsts

-- | Execute bytecode instructions and print the result
executeInstructions :: [Instr] -> IO ()
executeInstructions instrs = do
    putStrLn $ "[Bytecode] " ++ show instrs
    exec (baseState instrs []) >>= printVMResult

-- | Print VM execution result and exit with appropriate code
printVMResult :: VMState -> IO ()
printVMResult result@(VMState {stack = (VNumber (VInt r)) : _}) =
    putStrLn ("[Execution finished] Final VM state: " ++ show result) >> exitWithCode (fromIntegral r)
printVMResult result =
    putStrLn ("[Execution finished] Final VM state: " ++ show result ++
    "\n invalid return type") >> exitWithCode 1


-- | write bytecode to binary
writeToBinary :: String -> [Instr] -> IO ()
writeToBinary bName instr = catch (encodeFile bName instr) (\(exc :: IOException) -> hPutStrLn stderr ("Error writing binary :" <> show exc))

-- | Exit with the given code
exitWithCode :: Int -> IO ()
exitWithCode 0 = exitSuccess
exitWithCode n = exitWith (ExitFailure n)
