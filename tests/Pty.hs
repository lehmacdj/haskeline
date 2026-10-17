{-# LANGUAGE ForeignFunctionInterface #-}
-- This module is a quick-and-dirty way to run an executable
-- within a pseudoterminal and obtain its output.
-- 
-- It is not intended for general use.  In particular:
-- - It expects the output to be available quickly (<0.2s)
--   after each chunk of input.
-- - It expects each output chunk be no more than 4096 bytes.
module Pty (runCommandInPty) where

import System.Posix.Types
import System.Posix.Terminal
import System.Posix.Process
import System.Posix.Signals
import qualified Data.ByteString as B
import System.Posix.IO.ByteString
import Foreign.Marshal.Alloc
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr
import Control.Concurrent

-- Run the given command in a pseudoterminal, and return its output chunks.
-- Read the initial output, then feed the given input to it
-- one block at a time.
-- After each input, pause for 0.2s and then read as much output as possible.
--
-- The pause is the given number of microseconds (added in this
-- fork, see FORK.md: 20ms is too short for a program that runs another).
runCommandInPty :: Int -> String -> [String] -> Maybe [(String,String)]
        -> [B.ByteString] -> IO [B.ByteString]
runCommandInPty pause prog args env inputs = do
    (fd,pid) <- forkCommandInPty prog args env
    -- Block until the initial output from the program.
    -- After that, only use non-blocking reads.  Otherwise,
    -- we would hang the program in cases where the program has no output
    -- in between inputs.
    firstOutput <- readOutput pause fd
    setFdOption fd NonBlockingRead True
    restOfFirstOutput <- getOutput pause fd
    outputs <- mapM (inputOutput pause fd) inputs
    signalProcess killProcess pid
    _status <- getProcessStatus True False pid
    closeFd fd
    return ((firstOutput `B.append` restOfFirstOutput) : outputs)

inputOutput :: Int -> Fd -> B.ByteString -> IO B.ByteString
inputOutput pause fd input = do
    putInput fd input
    getOutput pause fd

putInput :: Fd -> B.ByteString -> IO ()
putInput fd input =
    B.useAsCStringLen input $ \(cstr,len) -> do
        written <- fdWriteBuf fd (castPtr cstr) (fromIntegral len)
        if written == 0
            then threadDelay 1000 >> putInput fd input
            else return ()

-- Read until no more output comes for the pause (this fork, see
-- FORK.md: a program may write what one input makes in several writes).
getOutput :: Int -> Fd -> IO B.ByteString
getOutput pause fd = loop B.empty
  where
    loop acc = do
        out <- readOutput pause fd
        if B.null out then return acc else loop (acc `B.append` out)

-- Pause, then read what output there is.
readOutput :: Int -> Fd -> IO B.ByteString
readOutput pause fd = do
            threadDelay pause
            allocaBytes (fromIntegral numBytes) $ \buf -> do
                num <- fdReadNonBlocking fd buf numBytes
                B.packCStringLen (castPtr buf, fromIntegral num)
  where
    numBytes = 4096


-- Unlike fdReadBuf, don't throw an error if nothing's immediately available.
-- Instead, just return empty output.
fdReadNonBlocking :: Fd -> Ptr CChar -> CSize -> IO CSsize
fdReadNonBlocking fd buf n = do
    num_read <- c_read fd buf n
    if num_read >= 0
        then return num_read
        else do
            e <- getErrno
            if e == eAGAIN
                then return 0
                else throwErrno "fdReadNonBlocking"

foreign import ccall "read" c_read :: Fd -> Ptr CChar -> CSize -> IO CSsize


-- returns the master Fd, and the pid for the subprocess.
forkCommandInPty :: String -> [String] -> Maybe [(String,String)]
                    -> IO (Fd,ProcessID)
forkCommandInPty prog args env = do
    (master,slave) <- openPseudoTerminal
    pid <- forkProcess $ do
                    closeFd master
                    loginTTY slave
                    executeFile prog False args env
    return (master,pid)


loginTTY :: Fd -> IO ()
loginTTY = throwErrnoIfMinus1_ "loginTTY" . login_tty

foreign import ccall login_tty :: Fd -> IO CInt

