{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VM.Syscall (executeSyscall) where

import DataStruct.Bytecode.Syscall (Syscall (..))
import DataStruct.VM (VMState (..))
import DataStruct.Bytecode.Value (Value(..))
import DataStruct.Bytecode.Number (Number(..))
import VM.Errors (ExecError (..))
import VM.Utils (makeIntValue, createList)
import Control.Exception (catch, throw, throwIO, IOException)
import GHC.IO.FD (openFile, FD (..), release, writeRawBufferPtr, readRawBufferPtr)
import qualified Data.Vector as V
import System.IO (IOMode (..))
import Foreign.C.String
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc

executeSyscall :: Syscall -> VMState -> IO VMState
-- Exit
executeSyscall Exit (VMState {stack = x:_}) = throwIO $ ExitException $ makeIntValue x

-- Print
executeSyscall (Print n) s@(VMState {stack, ip}) = case createList stack n of
    (l, st) -> f l >> pure (s {stack = st, ip = ip + 1})
    where
        f [] = return ()
        f (x:xs) = print x >> f xs

-- Open
executeSyscall Open s@(VMState {stack = (VList file) : xs, ip}) =
    catch
        (openFile (map toChar (V.toList file)) ReadWriteMode True >>= \((fd), _) -> pure (fromIntegral (fdFD fd)))
        (\(_ :: IOException) -> pure (-1)) >>=
    \fd -> pure (s{stack = VNumber (VInt fd) : xs, ip = ip + 1})
    where
        toChar (VNumber (VChar c)) = c
        toChar _ = throw $ InvalidCharConversion

-- Close
executeSyscall Close s@(VMState {stack = fd : xs, ip}) =
    release (FD (fromIntegral (makeIntValue fd)) 0) >> pure (s{stack = xs, ip = ip + 1})

-- Write
executeSyscall Write s@(VMState {stack = fd : v : xs, ip}) =
    catch
        (withCString stringV $ \
            ptr -> writeRawBufferPtr "" (FD (fromIntegral (makeIntValue fd)) 0) (castPtr ptr) 0 (fromIntegral (length stringV)))
        (\(_ :: IOException) -> pure (-1)) >>=
    \n -> pure (s{stack = VNumber (VInt (fromIntegral n)) : xs, ip = ip + 1})
    where stringV = show v

-- Read
executeSyscall Read s@(VMState {stack = fd : n : xs, ip}) =
  catch
    (allocaBytes len $ \ptr ->
        readRawBufferPtr "" (FD (fromIntegral (makeIntValue fd)) 0) ptr 0 (fromIntegral len)
        >>= \bytesRead -> peekCStringLen (castPtr ptr, bytesRead))
    (\(_ :: IOException) -> pure "")
  >>= \str ->
      pure (s {stack = VList (V.fromList (map (\c -> VNumber (VChar c)) str)) : xs, ip = ip + 1})
  where
    len = (makeIntValue n)
