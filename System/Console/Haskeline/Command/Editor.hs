-- | Editing the line in an external editor (added to the vendored copy, see
-- VENDORED.md).
module System.Console.Haskeline.Command.Editor (editInEditor) where

import System.Console.Haskeline.Command
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
            status <- withTerminalForEditor (runEditor path)
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

-- | Run @EDITOR@ on the file the way a shell runs @$EDITOR file@ (as git
-- does), so that the variable can hold arguments (@code --wait@). An
-- interrupt while the editor runs goes to the editor ('delegate_ctlc').
runEditor :: FilePath -> IO ExitCode
runEditor path = do
    variable <- lookupEnv "EDITOR"
    let editor = case variable of
            Just e | not (all isSpace e) -> e
            _ -> "vim"
    withCreateProcess (editorProcess editor) { delegate_ctlc = True }
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
