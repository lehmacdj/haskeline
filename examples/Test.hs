{-# LANGUAGE CPP #-}
module Main where

import System.Console.Haskeline
import System.Environment
import Control.Concurrent
import Control.Exception (finally, throwTo)
import Control.Monad (void)
#ifndef mingw32_HOST_OS
import Foreign.C (CInt(..), throwErrnoIfMinus1_)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import System.Posix.Signals
#endif

{--
Testing the line-input functions and their interaction with ctrl-c signals.

Usage:
./Test          (line input)
./Test chars    (character input)
./Test password (no masking characters)
./Test password \*
./Test initial  (use initial text in the prompt)

Added in this fork (see FORK.md), before the others:
./Test forked ...  (read the lines on a thread of its own, as ge does)
./Test unheld ...  (catch ctrl-c with a handler the GHC runtime doesn't know,
                    as ge does, rather than with withInterrupt)
--}

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

main :: IO ()
main = do
        allArgs <- getArgs
        let (forked, args1) = flag "forked" allArgs
            (unheld, args) = flag "unheld" args1
            run = do
                tid <- myThreadId
                interrupts <- if unheld
                    then installUnheldInterrupt tid >> return id
                    else return withInterrupt
                runInputT mySettings $ interrupts $ loop (inputFuncOf args) 0
        if forked
            then do
                done <- newEmptyMVar
                _ <- forkIO (run `finally` putMVar done ())
                takeMVar done
            else run
    where
        flag f (a:as) | a == f = (True, as)
        flag _ as = (False, as)
        inputFuncOf args = case args of
                ["chars"] -> fmap (fmap (\c -> [c])) . getInputChar
                ["password"] -> getPassword Nothing
                ["password", [c]] -> getPassword (Just c)
                ["initial"] -> flip getInputLineWithInitial ("left ", "right")
                _ -> getInputLine
        loop :: (String -> InputT IO (Maybe String)) -> Int -> InputT IO ()
        loop inputFunc n = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $ inputFunc (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "q" -> return ()
                Just s -> do
                            outputStrLn ("line " ++ show n ++ ":" ++ s)
                            loop inputFunc (n+1)

-- | Throw 'Interrupt' to the thread on ctrl-c, with the handler taken out of
-- the GHC runtime's table, as ge's installHandlerUnheld does (see its
-- comment there): a program that restores the handler through the
-- runtime then restores the wrong one.
installUnheldInterrupt :: ThreadId -> IO ()
#ifdef mingw32_HOST_OS
installUnheldInterrupt _ = return ()
#else
installUnheldInterrupt tid = do
    void $ installHandler sigINT (Catch (throwTo tid Interrupt)) Nothing
    allocaBytes 1024 $ \action -> do
        throwErrnoIfMinus1_ "sigaction" (c_sigaction sigINT nullPtr action)
        -- STG_SIG_IGN: the runtime takes the signal out of its table
        _ <- c_stg_sig_install sigINT (-2) nullPtr
        throwErrnoIfMinus1_ "sigaction" (c_sigaction sigINT action nullPtr)

foreign import ccall unsafe "sigaction"
    c_sigaction :: CInt -> Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "stg_sig_install"
    c_stg_sig_install :: CInt -> CInt -> Ptr () -> IO CInt
#endif
