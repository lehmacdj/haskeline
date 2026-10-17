-- | The kitty keyboard protocol (added to the vendored copy, see
-- VENDORED.md): while a line is read the terminal is asked to send keys
-- that it otherwise can't tell apart (escape and alt-key, return and
-- shift-return) as escape sequences of their own, which are read here.
--
-- <https://sw.kovidgoyal.net/kitty/keyboard-protocol/>; only the first
-- enhancement ("disambiguate escape codes") is asked for. A terminal without
-- the protocol ignores the request and sends keys as before.
module System.Console.Haskeline.KeyboardProtocol (
                        pushProtocol,
                        popProtocol,
                        setProtocolSwitch,
                        withoutProtocol,
                        CsiKey(..),
                        lexCsi
                        ) where

import Control.Exception (bracket_, mask_)
import Data.Bits ((.&.), testBit)
import Data.Char (chr, isDigit, isLetter, toUpper)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import System.Console.Haskeline.Key

-- | Have the terminal push, and pop, the protocol's enhancement flags.
pushProtocol, popProtocol :: String
pushProtocol = "\ESC[>1u"
popProtocol = "\ESC[<u"

-- | How to turn the protocol off and on again while it is on. The terminal
-- has one keyboard mode, which a program that haskeline runs (the editor of
-- Ctrl-G) or a shell that takes the terminal back (on ctrl-z) expects to be
-- the one it knows.
protocolSwitch :: IORef (Maybe (IO (), IO ()))
protocolSwitch = unsafePerformIO (newIORef Nothing)
{-# NOINLINE protocolSwitch #-}

setProtocolSwitch :: Maybe (IO (), IO ()) -> IO ()
setProtocolSwitch = writeIORef protocolSwitch

-- | Run the action with the protocol off, if it is on.
withoutProtocol :: IO a -> IO a
withoutProtocol act = do
    switch <- readIORef protocolSwitch
    case switch of
        Nothing -> act
        Just (off, on) -> bracket_ (mask_ off) (mask_ on) act

-- | What an escape sequence at the start of the input is.
data CsiKey = CsiKey Key -- ^ a key
            | CsiIgnored -- ^ a key haskeline has no use for, or a reply
                         -- from the terminal
            deriving Show

-- | Read a control sequence (@ESC [@ parameters final) at the start of the
-- input; 'Nothing' when the input doesn't start with a whole one.
lexCsi :: String -> Maybe (CsiKey, String)
lexCsi ('\ESC':'[':cs)
    | (params, final:rest) <- span isParam cs
    , final >= '@' && final <= '~'
        = Just (maybe CsiIgnored CsiKey (csiKey params final), rest)
  where
    isParam c = isDigit c || c `elem` ";:<=>?"
lexCsi _ = Nothing

csiKey :: String -> Char -> Maybe Key
csiKey params final
    | any (`elem` "<=>?") params = Nothing -- a reply from the terminal
    | otherwise = do
        base <- case final of
            'u' -> codepointKey (field 0 0)
            '~' -> tildeKey (field 0 0)
            'Z' -> Just (Key shiftOnly (KeyChar '\t')) -- shift-tab, legacy
            _ -> lookup final letterKeys
        modifyKey (field 1 1 - 1) base
  where
    fields = splitOn ';' params
    -- the first number of a field (the others are alternate keys or event
    -- types, which aren't asked for)
    field i def = case drop i fields of
        f:_ | ds@(_:_) <- takeWhile isDigit f -> read ds
        _ -> def :: Int
    shiftOnly = noModifier {hasShift = True}

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
    (x, _:rest) -> x : splitOn c rest
    (x, []) -> [x]

-- | Keys reported as @CSI 1 ; modifiers letter@ (or without parameters).
letterKeys :: [(Char, Key)]
letterKeys = map (fmap simpleKey)
    [ ('A', UpKey), ('B', DownKey), ('C', RightKey), ('D', LeftKey)
    , ('H', Home), ('F', End)
    , ('P', FunKey 1), ('Q', FunKey 2), ('S', FunKey 4)
    ]

-- | Keys reported as @CSI number ; modifiers ~@.
tildeKey :: Int -> Maybe Key
tildeKey n = simpleKey <$> lookup n
    [ (3, Delete), (5, PageUp), (6, PageDown), (7, Home), (8, End)
    , (11, FunKey 1), (12, FunKey 2), (13, FunKey 3), (14, FunKey 4)
    , (15, FunKey 5), (17, FunKey 6), (18, FunKey 7), (19, FunKey 8)
    , (20, FunKey 9), (21, FunKey 10), (23, FunKey 11), (24, FunKey 12)
    ]

-- | Keys reported as @CSI codepoint ; modifiers u@: a key that types text
-- is its (unshifted) character, other keys have numbers of their own.
codepointKey :: Int -> Maybe Key
codepointKey n = case n of
    27 -> char '\ESC'
    13 -> char '\n'
    9 -> char '\t'
    127 -> Just (simpleKey Backspace)
    8 -> Just (simpleKey Backspace)
    _ | n >= 57399 && n <= 57408 -> char (chr (n - 57399 + fromEnum '0'))
    _ | n >= 57344 && n <= 63743 -> keypadKey n -- kitty's private use area
    _ | n > 0 && n <= 0x10FFFF -> char (chr n)
    _ -> Nothing
  where
    char = Just . simpleChar

keypadKey :: Int -> Maybe Key
keypadKey n = lookup n
    $ [ (57409, simpleChar '.'), (57410, simpleChar '/')
      , (57411, simpleChar '*'), (57412, simpleChar '-')
      , (57413, simpleChar '+'), (57414, simpleChar '\n')
      , (57415, simpleChar '='), (57416, simpleChar ',')
      ] ++ map (fmap simpleKey)
      [ (57417, LeftKey), (57418, RightKey), (57419, UpKey)
      , (57420, DownKey), (57421, PageUp), (57422, PageDown)
      , (57423, Home), (57424, End), (57426, Delete)
      ]

-- | Apply the modifiers (the bits of the protocol's modifier number less
-- one) to a key the way a terminal without the protocol would send it:
-- ctrl and a letter is a control character, shift and a letter is a capital,
-- ctrl changes nothing about return, tab, escape or backspace, and shift
-- nothing about escape or backspace. A key
-- with super or hyper is ignored; caps lock and num lock are.
modifyKey :: Int -> Key -> Maybe Key
modifyKey bits (Key _ base)
    | testBit bits 3 || testBit bits 4 = Nothing -- super, hyper
    | otherwise = Just $ case base of
        KeyChar c -> modifyChar c
        Backspace -> Key mods {hasControl = False, hasShift = False} Backspace
        _ -> Key mods base
  where
    m = bits .&. 63
    shift = testBit m 0
    alt = testBit m 1 || testBit m 5 -- alt or meta
    ctrl = testBit m 2
    mods = Modifier {hasControl = ctrl, hasMeta = alt, hasShift = shift}
    modifyChar c
        | ctrl, Just c' <- controlFor (upperIfShift c)
            = Key noModifier {hasMeta = alt} (KeyChar c')
        | c == '\ESC' = Key noModifier {hasMeta = alt} (KeyChar c)
        -- shift and return stays apart from return, as the protocol is
        -- there for
        | c `elem` "\n\t" = Key mods {hasControl = False} (KeyChar c)
        | shift && isLetter c
            = Key mods {hasShift = False} (KeyChar (toUpper c))
        | otherwise = Key mods (KeyChar c)
    upperIfShift c = if shift then toUpper c else c

-- | The control character that ctrl and the given key send without the
-- protocol (as kitty does). Ctrl-m and ctrl-j are return, as the terminal's
-- input translation (ICRNL) makes them.
controlFor :: Char -> Maybe Char
controlFor c
    | c >= 'a' && c <= 'z' = controlFor (toUpper c)
    | c == 'M' || c == 'J' = Just '\n'
    | c >= '@' && c <= '_' = Just (toEnum (fromEnum c - 64))
    | otherwise = lookup c
        [ (' ', '\NUL'), ('2', '\NUL'), ('3', '\ESC'), ('4', '\FS')
        , ('5', '\GS'), ('6', '\RS'), ('7', '\US'), ('/', '\US')
        , ('8', '\DEL'), ('?', '\DEL'), ('~', '\RS')
        ]
