{-# LANGUAGE CApiFFI #-}

module System.Console.Haskeline.Backend.Posix (
                        withPosixGetEvent,
                        posixLayouts,
                        tryGetLayouts,
                        PosixT,
                        Handles(),
                        ehIn,
                        ehOut,
                        mapLines,
                        stdinTTYHandles,
                        ttyHandles,
                        posixRunTerm,
                        fileRunTerm
                 ) where

import Foreign
import Foreign.C.Types
import qualified Data.Map as Map
import System.Posix.Terminal hiding (Interrupt)
import qualified System.Posix.Terminal as Terminal
import Control.Exception (throwTo)
import Control.Monad
import Control.Monad.Catch (MonadMask, handle, finally, bracket_)
import Control.Concurrent.STM
import Control.Concurrent hiding (throwTo)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Char (isControl)
import System.Posix.Signals.Exts
import System.Posix.Types(Fd(..))
import Data.Foldable (foldl')
import System.IO
import System.Environment

import System.Console.Haskeline.Monads
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term as Term
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.KeyboardProtocol

import System.Console.Haskeline.Backend.Posix.Encoder

import GHC.IO.FD (fdFD)
import Data.Typeable (cast)
import System.IO.Error
import GHC.IO.Exception
import GHC.IO.Handle.Types hiding (getState)
import GHC.IO.Handle.Internals
import System.Posix.Internals (FD)

#if defined(USE_TERMIOS_H) || defined(__ANDROID__)
#include <termios.h>
#endif
#include <sys/ioctl.h>

#include <HsBaseConfig.h>

-----------------------------------------------
-- Input/output handles
data Handles = Handles {hIn, hOut :: ExternalHandle
                        , closeHandles :: IO ()}

ehIn, ehOut :: Handles -> Handle
ehIn = eH . hIn
ehOut = eH . hOut

-------------------
-- Window size

#if !defined(HAVE_TERMIOS_H)
posixLayouts :: Handles -> [IO (Maybe Layout)]
posixLayouts _ = error "System.Console.Haskeline.Backend.Posix.posixLayouts"
#else
foreign import capi "sys/ioctl.h ioctl" ioctl :: FD -> CULong -> Ptr a -> IO CInt

posixLayouts :: Handles -> [IO (Maybe Layout)]
posixLayouts h = [ioctlLayout $ ehOut h, envLayout]

ioctlLayout :: Handle -> IO (Maybe Layout)
ioctlLayout h = allocaBytes (#size struct winsize) $ \ws -> do
                fd <- unsafeHandleToFD h
                ret <- ioctl fd (#const TIOCGWINSZ) ws
                rows :: CUShort <- (#peek struct winsize,ws_row) ws
                cols :: CUShort <- (#peek struct winsize,ws_col) ws
                if ret >= 0
                    then return $ Just Layout {height=fromEnum rows,width=fromEnum cols}
                    else return Nothing

#endif

unsafeHandleToFD :: Handle -> IO FD
unsafeHandleToFD h =
  withHandle_ "unsafeHandleToFd" h $ \Handle__{haDevice=dev} -> do
  case cast dev of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "unsafeHandleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> return (fdFD fd)

envLayout :: IO (Maybe Layout)
envLayout = handle (\(_::IOException) -> return Nothing) $ do
    -- note the handle catches both undefined envs and bad reads
    r <- getEnv "ROWS"
    c <- getEnv "COLUMNS"
    return $ Just $ Layout {height=read r,width=read c}

tryGetLayouts :: [IO (Maybe Layout)] -> IO Layout
tryGetLayouts [] = return Layout {height=24,width=80}
tryGetLayouts (f:fs) = do
    ml <- f
    case ml of
        Just l | height l > 2 && width l > 2 -> return l
        _ -> tryGetLayouts fs


--------------------
-- Key sequences

getKeySequences :: (MonadIO m, MonadReader Prefs m)
        => Handle -> [(String,Key)] -> m (TreeMap Char Key)
getKeySequences h tinfos = do
    sttys <- liftIO $ sttyKeys h
    customKeySeqs <- getCustomKeySeqs
    -- note ++ acts as a union; so the below favors sttys over tinfos
    return $ listToTree
        $ ansiKeys ++ tinfos ++ sttys ++ customKeySeqs
  where
    getCustomKeySeqs = do
        kseqs <- asks customKeySequences
        termName <- liftIO $ handle (\(_::IOException) -> return "") (getEnv "TERM")
        let isThisTerm = maybe True (==termName)
        return $ map (\(_,cs,k) ->(cs,k))
            $ filter (\(kseqs',_,_) -> isThisTerm kseqs')
            $ kseqs


ansiKeys :: [(String, Key)]
ansiKeys = [("\ESC[D",  simpleKey LeftKey)
            ,("\ESC[C",  simpleKey RightKey)
            ,("\ESC[A",  simpleKey UpKey)
            ,("\ESC[B",  simpleKey DownKey)
            ,("\b",      simpleKey Backspace)
            -- ctrl-left/right aren't a standard
            -- part of terminfo, but enough people have complained
            -- that I've decided to hard-code them in.
            -- (Note they will be overridden by terminfo or .haskeline.)
            -- These appear to be the most common bindings:
            -- xterm:
            ,("\ESC[1;5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[1;5C", ctrlKey $ simpleKey RightKey)
            -- Terminal.app:
            ,("\ESC[5D", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[5C", ctrlKey $ simpleKey RightKey)
            -- rxvt: (Note: these will be superseded by e.g. xterm-color,
            -- which uses them as regular arrow keys.)
            ,("\ESC[OD", ctrlKey $ simpleKey LeftKey)
            ,("\ESC[OC", ctrlKey $ simpleKey RightKey)
            -- shift-return, which terminals only tell apart from return
            -- when set up to (added in this fork, see FORK.md):
            -- the CSI u encoding (the kitty keyboard protocol, "fixterms"),
            -- which is also what terminals are usually configured to send
            ,("\ESC[13;2u", shiftReturn)
            -- xterm's modifyOtherKeys
            ,("\ESC[27;2;13~", shiftReturn)
            ]
  where
    shiftReturn = Key noModifier {hasShift = True} (KeyChar '\n')


sttyKeys :: Handle -> IO [(String, Key)]
sttyKeys h = do
    fd <- unsafeHandleToFD h
    attrs <- getTerminalAttributes (Fd fd)
    let getStty (k,c) = do {str <- controlChar attrs k; return ([str],c)}
    return $ catMaybes $ map getStty [(Erase,simpleKey Backspace),(Kill,simpleKey KillLine)]

newtype TreeMap a b = TreeMap (Map.Map a (Maybe b, TreeMap a b))
                        deriving Show

emptyTreeMap :: TreeMap a b
emptyTreeMap = TreeMap Map.empty

insertIntoTree :: Ord a => ([a], b) -> TreeMap a b -> TreeMap a b
insertIntoTree ([],_) _ = error "Can't insert empty list into a treemap!"
insertIntoTree ((c:cs),k) (TreeMap m) = TreeMap (Map.alter f c m)
    where
        alterSubtree = insertIntoTree (cs,k)
        f Nothing = Just $ if null cs
                            then (Just k, emptyTreeMap)
                            else (Nothing, alterSubtree emptyTreeMap)
        f (Just (y,t)) = Just $ if null cs
                                    then (Just k, t)
                                    else (y, alterSubtree t)

listToTree :: Ord a => [([a],b)] -> TreeMap a b
listToTree = foldl' (flip insertIntoTree) emptyTreeMap

-- for debugging '
mapLines :: (Show a, Show b) => TreeMap a b -> [String]
mapLines (TreeMap m) = let
    m2 = Map.map (\(k,t) -> show k : mapLines t) m
    in concatMap (\(k,ls) -> show k : map (' ':) ls) $ Map.toList m2

lexKeys :: TreeMap Char Key -> [Char] -> [Key]
lexKeys _ [] = []
lexKeys baseMap cs
    | Just (k,ds) <- lookupChars baseMap cs
            = k : lexKeys baseMap ds
-- the kitty keyboard protocol's sequences, and other control sequences that
-- are whole, which are ignored rather than read as the keys that make them up
-- (this fork, see FORK.md)
lexKeys baseMap cs
    | Just (csi,ds) <- lexCsi cs = case csi of
        CsiKey k -> asLegacyKey baseMap k : lexKeys baseMap ds
        CsiIgnored -> lexKeys baseMap ds
lexKeys baseMap ('\ESC':cs)
-- TODO: what's the right thing ' to do here?
    | k:ks <- lexKeys baseMap cs
            = metaKey k : ks
lexKeys baseMap (c:cs) = simpleChar c : lexKeys baseMap cs

-- | A character from the protocol is read as the same character typed
-- without it would be (the terminal's erase and kill characters, the
-- sequences of @~/.haskeline@).
asLegacyKey :: TreeMap Char Key -> Key -> Key
asLegacyKey baseMap k@(Key m (KeyChar c))
    | not (hasControl m) && not (hasShift m)
    , [k'] <- lexKeys baseMap [c]
        = if hasMeta m then metaKey k' else k'
    | otherwise = k
asLegacyKey _ k = k

lookupChars :: TreeMap Char Key -> [Char] -> Maybe (Key,[Char])
lookupChars _ [] = Nothing
lookupChars (TreeMap tm) (c:cs) = case Map.lookup c tm of
    Nothing -> Nothing
    Just (Nothing,t) -> lookupChars t cs
    Just (Just k, t@(TreeMap tm2))
                | not (null cs) && not (Map.null tm2) -- ?? lookup d tm2?
                    -> lookupChars t cs
                | otherwise -> Just (k, cs)

-----------------------------

withPosixGetEvent :: (MonadIO m, MonadMask m, MonadReader Prefs m)
        => TChan Event -> Handles -> [(String,Key)] -> Bool
                -> (m Event -> m a) -> m a
withPosixGetEvent eventChan h termKeys canUseProtocol f = wrapTerminalOps h $ do
    baseMap <- getKeySequences (ehIn h) termKeys
    useProtocol <- (canUseProtocol &&) <$> asks kittyKeyboardProtocol
    (if useProtocol then withKeyboardProtocol h else id)
        $ withWindowHandler eventChan
        $ f $ liftIO $ getEvent (ehIn h) baseMap eventChan

-- | Have the terminal use the kitty keyboard protocol (this fork, see
-- FORK.md).
withKeyboardProtocol :: (MonadIO m, MonadMask m) => Handles -> m a -> m a
withKeyboardProtocol h = bracket_ (liftIO on) (liftIO off)
  where
    send s = hPutStr (ehOut h) s >> hFlush (ehOut h)
    on = do
        send pushProtocol
        setProtocolSwitch $ Just (send popProtocol, send pushProtocol)
    off = setProtocolSwitch Nothing >> send popProtocol

withWindowHandler :: (MonadIO m, MonadMask m) => TChan Event -> m a -> m a
withWindowHandler eventChan = withHandler windowChange $
    Catch $ atomically $ writeTChan eventChan WindowResize

withSigIntHandler :: (MonadIO m, MonadMask m) => m a -> m a
withSigIntHandler f = do
    tid <- liftIO myThreadId
    withHandler keyboardSignal
            (Catch (throwTo tid Interrupt))
            f

withHandler :: (MonadIO m, MonadMask m) => Signal -> Handler -> m a -> m a
withHandler signal handler f = do
    old_handler <- liftIO $ installHandler signal handler Nothing
    f `finally` liftIO (installHandler signal old_handler Nothing)

getEvent :: Handle -> TreeMap Char Key -> TChan Event -> IO Event
getEvent h baseMap = keyEventLoop $ do
        cs <- getBlockOfChars h
        noteProtocolKeys cs
        signalled <- raiseSignalOfKeys h $ lexKeys baseMap cs
        return $ case signalled of
            -- reading on is where an interrupt is expected (it moves to the
            -- next line)
            Signalled -> []
            Keys ks -> [KeyInput ks]

data SignalledOrKeys = Signalled | Keys [Key]

-- | The terminal makes the interrupt, quit and suspend characters signals
-- (when ISIG is set), so they reach haskeline only from the kitty keyboard
-- protocol, which sends them as keys: raise the signal the terminal would
-- have, and as it does drop the input that came with it (this fork, see
-- FORK.md). The program is stopped, or may stop, with the terminal's
-- keyboard mode as the shell knows it.
raiseSignalOfKeys :: Handle -> [Key] -> IO SignalledOrKeys
raiseSignalOfKeys h ks
    | not (any (isJust . plainControl) ks) = return (Keys ks)
    | otherwise = do
        fd <- unsafeHandleToFD h
        attrs <- getTerminalAttributes (Fd fd)
        let signals = catMaybes
                [ fmap (\c -> (c, sig)) (controlChar attrs cc)
                | (cc, sig) <- [ (Terminal.Interrupt, keyboardSignal)
                               , (Terminal.Quit, keyboardTermination)
                               , (Terminal.Suspend, keyboardStop) ] ]
            signalOf k = plainControl k >>= flip lookup signals
        case mapMaybe signalOf ks of
            sig:_ | terminalMode KeyboardInterrupts attrs -> do
                withoutProtocol $ do
                    signalForeground (Fd fd) sig
                    -- another thread of the program can go on for a moment
                    -- before the stop: turning the protocol back on then
                    -- would give it to the shell. Stopped, the program
                    -- turns it back on once continued. (An event that ends
                    -- this reader meanwhile, a resize say, cuts the wait
                    -- short.)
                    when (sig == keyboardStop) $ threadDelay 100000
                return Signalled
            _ -> return (Keys ks)
  where
    plainControl (Key m (KeyChar c)) | m == noModifier && isControl c = Just c
    plainControl _ = Nothing

-- | Send the signal to the terminal's foreground process group, as the
-- terminal does: to the other programs of a pipeline (@ge | tee log@) as
-- well. Only to this program when the group isn't known.
signalForeground :: Fd -> Signal -> IO ()
signalForeground fd sig = do
    sent <- tryIOError $ do
        group <- getTerminalProcessGroupID fd
        if group > 0 then signalProcessGroup sig group
            else ioError (userError "no foreground process group")
    either (const (raiseSignal sig)) return sent

-- Read at least one character of input, and more if immediately
-- available.  In particular the characters making up a control sequence
-- will all be available at once, so they can be processed together
-- (with Posix.lexKeys).  A control sequence (@ESC [@) can still arrive in
-- two reads, over ssh say, and with the kitty keyboard protocol every key
-- with ctrl or alt is one; when the input ends partway through one (see
-- 'endsInPartialKey'), wait a moment for the rest (added in this fork, see
-- FORK.md).
getBlockOfChars :: Handle -> IO String
getBlockOfChars h = do
    c <- hGetChar h
    loop [c]
  where
    loop cs = do
        isReady <- hReady h
        partial <- if isReady then return False
            else endsInPartialKey (reverse cs)
        more <- if partial then hWaitForInput h partialCsiWaitMs
            else return isReady
        if not more
            then return $ reverse cs
            else do
                    c <- hGetChar h
                    loop (c:cs)

-- | How long to wait for the rest of a control sequence, in milliseconds.
partialCsiWaitMs :: Int
partialCsiWaitMs = 100

stdinTTYHandles, ttyHandles :: MaybeT IO Handles
stdinTTYHandles = do
    isInTerm <- liftIO $ hIsTerminalDevice stdin
    guard isInTerm
    h <- openTerm WriteMode
    -- Don't close stdin, since a different part of the program may use it later.
    return Handles
            { hIn = externalHandle stdin
            , hOut = h
            , closeHandles = hClose $ eH h
            }

ttyHandles = do
    -- Open the input and output as two separate Handles, since they need
    -- different buffering.
    h_in <- openTerm ReadMode
    h_out <- openTerm WriteMode
    return Handles
            { hIn = h_in
            , hOut = h_out
            , closeHandles = hClose (eH h_in) >> hClose (eH h_out)
            }

openTerm :: IOMode -> MaybeT IO ExternalHandle
openTerm mode = handle (\(_::IOException) -> mzero)
            $ liftIO $ openInCodingMode "/dev/tty" mode


posixRunTerm ::
    Handles
    -> [IO (Maybe Layout)]
    -> [(String,Key)]
    -> Bool -- ^ whether to use the kitty keyboard protocol
    -> (forall m b . (MonadIO m, MonadMask m) => m b -> m b)
    -> (forall m . (MonadMask m, CommandMonad m) => EvalTerm (PosixT m))
    -> IO RunTerm
posixRunTerm hs layoutGetters keys useProtocol wrapGetEvent evalBackend = do
    ch <- newTChanIO
    fileRT <- posixFileRunTerm hs
    return fileRT
                { termOps = Left TermOps
                            { getLayout = tryGetLayouts layoutGetters
                            , withGetEvent = wrapGetEvent
                                            . withPosixGetEvent ch hs
                                                keys useProtocol
                            , saveUnusedKeys = saveKeys ch
                            , evalTerm = mapEvalTerm
                                            (runPosixT hs) lift evalBackend
                            , externalPrint = atomically . writeTChan ch . ExternalPrint
                            }
                , closeTerm = do
                    flushEventQueue (putStrOut fileRT) ch
                    closeTerm fileRT
                }

type PosixT m = ReaderT Handles m

runPosixT :: Handles -> PosixT m a -> m a
runPosixT h = runReaderT' h

fileRunTerm :: Handle -> IO RunTerm
fileRunTerm h_in = posixFileRunTerm Handles
                        { hIn = externalHandle h_in
                        , hOut = externalHandle stdout
                        , closeHandles = return ()
                        }

posixFileRunTerm :: Handles -> IO RunTerm
posixFileRunTerm hs = do
    return RunTerm
                { putStrOut = \str -> withCodingMode (hOut hs) $ do
                                        hPutStr (ehOut hs) str
                                        hFlush (ehOut hs)
                , closeTerm = closeHandles hs
                , wrapInterrupt = withSigIntHandler
                , termOps = let h_in = ehIn hs
                            in Right FileOps
                          { withoutInputEcho = bracketSet (hGetEcho h_in)
                                                          (hSetEcho h_in)
                                                          False
                          , wrapFileInput = withCodingMode (hIn hs)
                          , getLocaleChar = guardedEOF hGetChar h_in
                          , maybeReadNewline = hMaybeReadNewline h_in
                          , getLocaleLine = guardedEOF hGetLine h_in
                          }
                }

-- NOTE: If we set stdout to NoBuffering, there can be a flicker effect when many
-- characters are printed at once.  We'll keep it buffered here, and let the Draw
-- monad manually flush outputs that don't print a newline.
wrapTerminalOps :: (MonadIO m, MonadMask m) => Handles -> m a -> m a
wrapTerminalOps hs =
    bracketSet (hGetBuffering h_in) (hSetBuffering h_in) NoBuffering
    -- TODO: block buffering?  Certain \r and \n's are causing flicker...
    -- - moving to the right
    -- - breaking line after offset widechar?
    . bracketSet (hGetBuffering h_out) (hSetBuffering h_out) LineBuffering
    . bracketSet (hGetEcho h_in) (hSetEcho h_in) False
    . withCodingMode (hIn hs)
    . withCodingMode (hOut hs)
  where
    h_in = ehIn hs
    h_out = ehOut hs
