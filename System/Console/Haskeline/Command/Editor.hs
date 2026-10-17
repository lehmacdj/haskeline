-- | Editing the line in an external editor (added in this fork, see
-- FORK.md).
module System.Console.Haskeline.Command.Editor (editInEditor) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.KeyboardProtocol (withoutProtocol)
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads

import Control.Exception (IOException)
import qualified Control.Exception as E
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO
import System.Process
#ifndef MINGW
import Foreign.C (CInt(..), CULong(..), throwErrnoIfMinus1_)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
#endif

-- | Open the line in the editor @EDITOR@ (@vim@ when it is not set) and make
-- what the editor saved the line, with the cursor at its end. The newlines
-- at the end are dropped and the others stay, since a line can have newlines
-- in it. When the editor fails the line stays as it was and the bell rings.
editInEditor :: (MonadIO m, Save s) => Command m s s
editInEditor s = do
    edited <- CmdM (Result <$> liftIO (runEditorOn (toResult (save s))))
    case edited of
        Nothing -> effect RingBell >> return s
        Just line -> setState (restore (insertString line emptyIM))

-- | Write the text to a temporary file, run the editor on it and read it
-- back; 'Nothing' when anything fails, the editor included.
runEditorOn :: String -> IO (Maybe String)
runEditorOn text = do
    tmp <- getTemporaryDirectory
    outcome <- E.try $ E.bracket (openTempFile tmp "line.txt") cleanUp $
        \(path, h) -> do
            hSetEncoding h utf8
            hPutStr h text
            hClose h
            status <- withoutProtocol $ withTerminalForEditor (runEditor path)
            case status of
                ExitSuccess -> Just . asLine <$> readUtf8 path
                ExitFailure _ -> return Nothing
    return $ either (\(_ :: IOException) -> Nothing) id outcome
  where
    cleanUp (path, h) = hClose h `E.finally` removeFile path
    asLine = filter (/= '\r') . dropWhileEnd (`elem` "\r\n")

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    length contents `seq` return contents

-- | Run the editor on the file the way a shell runs @$EDITOR file@ (as git
-- does), so that the variable can hold arguments (@code --wait@). Ctrl-c
-- and ctrl-\ while the editor runs go to the editor only: an editor they
-- end has failed.
runEditor :: FilePath -> IO ExitCode
runEditor path = do
    variable <- lookupEnv "EDITOR"
    let editor = case variable of
            Just e | not (all isSpace e) -> e
            _ -> "vim"
    holdingInterrupts $ withCreateProcess (editorProcess editor)
        $ \_ _ _ -> waitForProcess
  where
#ifdef MINGW
    editorProcess editor = shell (editor ++ " \"" ++ path ++ "\"")
#else
    editorProcess editor =
        proc "/bin/sh" ["-c", editor ++ " \"$@\"", editor, path]
#endif

-- | While the line is read the terminal does not echo and passes on each
-- key as it is typed; give the editor the terminal as a shell would, and
-- take it back after.
withTerminalForEditor :: IO a -> IO a
withTerminalForEditor run = do
    isTerminal <- hIsTerminalDevice stdin
    if not isTerminal
        then run
        else E.bracket
            ((,) <$> hGetEcho stdin <*> hGetBuffering stdin)
            (\(echo, buffering) ->
                hSetBuffering stdin buffering >> hSetEcho stdin echo)
            (\_ -> do
                hSetBuffering stdin LineBuffering
                hSetEcho stdin True
                run)

-- | Let ctrl-c and ctrl-\ (SIGINT and SIGQUIT) go by while the action
-- runs, as a shell does while a program it started has the terminal. Not
-- 'delegate_ctlc': that saves and restores the handlers through the GHC
-- runtime's table, which doesn't know a handler that a program installed
-- without it (as ge does), and so restores the wrong one; and it rethrows
-- an interrupt that ended the editor as 'UserInterrupt'. See
-- @cbits/h_signals.c@.
holdingInterrupts :: IO a -> IO a
#ifdef MINGW
holdingInterrupts = id
#else
holdingInterrupts act = do
    size <- c_held_signals_size
    allocaBytes (fromIntegral size) $ \held -> E.bracket_
        (throwErrnoIfMinus1_ "sigaction" (c_hold_interrupts held))
        (throwErrnoIfMinus1_ "sigaction" (c_release_interrupts held))
        act

foreign import ccall unsafe "haskeline_held_signals_size"
    c_held_signals_size :: IO CULong

foreign import ccall unsafe "haskeline_hold_interrupts"
    c_hold_interrupts :: Ptr () -> IO CInt

foreign import ccall unsafe "haskeline_release_interrupts"
    c_release_interrupts :: Ptr () -> IO CInt
#endif
