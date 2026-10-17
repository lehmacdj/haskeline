{-# LANGUAGE OverloadedStrings #-}
-- Requirements:
-- - (none for ~/.haskeline: the tests set HOME to tests/homes/)
-- - On Mac OS X, the "dumb term" test may fail.
--   In particular, the line "* UTF-8" which makes locale_charset()
--   always return UTF-8; otherwise we can't test latin-1.
-- - NB: Window size isn't provided by screen so it's picked up from
--   terminfo or defaults (either way: 80x24), rather than the user's
--   terminal.
module Main where

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Monoid ((<>))
import System.Exit (exitFailure)
import System.Info (os)
import System.Process (readProcess)
import System.Posix.Directory (getWorkingDirectory)
import Control.Concurrent (threadDelay)
import Test.HUnit

import RunTTY

legacyEncoding :: Bool
legacyEncoding = False

-- Generally we want the legacy and new backends to perform the same.
-- The only two differences I'm aware of are:
-- 1. base decodes invalid bytes as '\65533', but legacy decodes them as '?'
-- 2. if there's an incomplete sequence and no more input immediately
--   available (but not eof), then base will pause to wait for more input,
--   whereas legacy will immediately stop.
whenLegacy :: BC.ByteString -> BC.ByteString
whenLegacy s = if legacyEncoding then s else B.empty

main :: IO ()
main = do
    -- forkProcess needs an absolute path to the binary.
    p <- head . lines <$> readProcess "which" ["haskeline-examples-Test"] ""
    cwd <- getWorkingDirectory
    -- without HOME haskeline finds the home directory of whoever runs the
    -- tests, and their ~/.haskeline (this fork, see FORK.md)
    let i = withHome cwd "default" $ setTerm "xterm"
            Invocation {
                prog = p,
                progArgs = [],
                runInTTY = True,
                environment = [],
                pause = 20000
            }
    result <- runTestTT $ test
        [interactionTests i, fileStyleTests i, forkTests cwd i]
    when (errors result > 0 || failures result > 0) exitFailure

interactionTests :: Invocation -> Test
interactionTests i = "interaction" ~: test
    [ unicodeEncoding i
    , unicodeMovement i
    , tabCompletion i
    , incorrectInput i
    , historyTests i
    , inputChar $ setCharInput i
    , dumbTests $ setTerm "dumb" i
    ]

unicodeEncoding :: Invocation -> Test
unicodeEncoding i = "Unicode encoding (valid)" ~:
    [ utf8Test i [utf8 "xαβγy"]
        [prompt 0, utf8 "xαβγy"]
    , utf8Test i [utf8 "a\n", "quit\n"]
        [ prompt 0
        , utf8 "a" <> end
            <> output 0 (utf8 "a") <> prompt 1
        , utf8 "quit" <> end
        ]
    , utf8Test i [utf8 "xαβyψ안기q영\n", "quit\n"]
        [ prompt 0
        , utf8 "xαβyψ안기q영" <> end
            <> output 0 (utf8 "xαβyψ안기q영") <> prompt 1
        , utf8 "quit" <> end
        ]
    -- test buffering: 32 bytes is in middle of a char encoding,
    -- also test long paste
    , "multipleLines" ~: utf8Test i [l1 <> "\n" <> l1]
        [ prompt 0
        , l1 <> end <> output 0 l1 <> prompt 1 <> l1]
    ]
  where
    l1 = utf8 $ T.replicate 30 "안" -- three bytes, width 60

unicodeMovement :: Invocation -> Test
unicodeMovement i = "Unicode movement" ~:
    [ "separate" ~: utf8Test i [utf8 "α", utf8 "\ESC[Dx"]
        [prompt 0, utf8 "α", utf8 "\bxα\b"]
    , "coalesced" ~: utf8Test i [utf8 "α\ESC[Dx"]
        [prompt 0, utf8 "xα\b"]
    , "lineWrap" ~: utf8Test i
        [ utf8 longWideChar
        , raw [1]
        , raw [5]
        ]
        [prompt 0, utf8 lwc1 <> wrap <> utf8 lwc2 <> wrap <> utf8 lwc3
        , cr <> "\ESC[2A\ESC[2C"
        , cr <> nl <> nl <> "\ESC[22C"
        ]

    ]
  where
    longWideChar = T.concat $ replicate 30 $ "안기영"
    (lwc1,lwcs1) = T.splitAt ((80-2)`div`2) longWideChar
    (lwc2,lwcs2) = T.splitAt (80`div`2) lwcs1
    (lwc3,_lwcs3) = T.splitAt (80`div`2) lwcs2
    -- lwc3 has length 90 - (80-2)/2 - 80/2 = 11,
    -- so the last line as wide width 2*11=22.

tabCompletion :: Invocation -> Test
tabCompletion i = "tab completion" ~:
    [ utf8Test i [ utf8 "tests/dummy-μ\t\t" ]
        [ prompt 0, utf8 "tests/dummy-μασ/"
            <> nl <> utf8 "ςερτ  bar" <> nl
            <> prompt' 0 <> utf8 "tests/dummy-μασ/"
        ]
    ]

incorrectInput :: Invocation -> Test
incorrectInput i = "incorrect input" ~:
    [ utf8Test i [ utf8 "x" <> raw [206] ]  -- needs one more byte
        -- non-legacy encoder ignores the "206" since it's still waiting
        -- for more input.
        [ prompt 0, utf8 "x" <> whenLegacy err ]
    , utf8Test i [ raw [206] <> utf8 "x" ]
        -- 'x' is not valid after '\206', so both the legacy and
        -- non-legacy encoders should handle the "x" correctly.
        [ prompt 0, err <> utf8 "x"]
    , utf8Test i [ raw [236,149] <> utf8 "x" ] -- needs one more byte
        [prompt 0, err <> err <> utf8 "x"]
    ]

historyTests :: Invocation -> Test
historyTests i =  "history encoding" ~:
    [ utf8TestValidHist i [ "\ESC[A" ]
        [prompt 0, utf8 "abcα" ]
    , utf8TestInvalidHist i [ "\ESC[A" ]
        -- NB: this is decoded by either utf8-string or base;
        -- either way they produce \65533 instead of '?'.
        [prompt 0, utf8 "abcα\65533x\65533x\65533" ]
    ] ++
    -- In latin-1: read errors as utf-8 '\65533', display as '?'.
    -- Skipped on macOS (this fork, see FORK.md): GHC's locale
    -- encoding there is UTF-8 whatever LANG says, so the test can't run in
    -- latin-1 (it failed before the copy was changed).
    [ latin1TestInvalidHist i  [ "\ESC[A" ]
        [prompt 0, utf8 "abc??x?x?" ]
    | os /= "darwin"
    ]

invalidHist :: BC.ByteString
invalidHist =  utf8 "abcα"
              `B.append` raw [149] -- invalid start of UTF-8 sequence
              `B.append` utf8 "x"
              `B.append` raw [206] -- incomplete start
              `B.append` utf8 "x"
              -- incomplete at end of file
              `B.append` raw [206]

validHist :: BC.ByteString
validHist = utf8 "abcα"

inputChar :: Invocation -> Test
inputChar i = "getInputChar" ~:
    [ utf8Test i [utf8 "xαβ"]
        [ prompt 0, utf8 "x" <> end <> output 0 (utf8 "x")
          <> prompt 1 <> utf8 "α" <> end <> output 1 (utf8 "α")
          <> prompt 2 <> utf8 "β" <> end <> output 2 (utf8 "β")
          <> prompt 3
        ]
    , "bad encoding (separate)" ~:
        utf8Test i [utf8 "α", raw [149], utf8 "x", raw [206]]
        [ prompt 0, utf8 "α" <> end <> output 0 (utf8 "α") <> prompt 1
        , err <> end <> output 1 err <> prompt 2
        , utf8 "x" <> end <> output 2 (utf8 "x") <> prompt 3
        , whenLegacy (err <> end <> output 3 err <> prompt 4)
        ]
    , "bad encoding (together)" ~:
        utf8Test i [utf8 "α" <> raw [149] <> utf8 "x" <> raw [206]]
        [ prompt 0, utf8 "α" <> end <> output 0 (utf8 "α")
        <> prompt 1 <> err <> end <> output 1 err
        <> prompt 2 <> utf8 "x" <> end <> output 2 (utf8 "x")
        <> prompt 3 <> whenLegacy (err <> end <> output 3 err <> prompt 4)
        ]
    , utf8Test i [raw [206]] -- incomplete
        [ prompt 0, whenLegacy (utf8 "?" <> end <> output 0 (utf8 "?"))
        <> whenLegacy (prompt 1)
        ]
    ]

setCharInput :: Invocation -> Invocation
setCharInput i = i { progArgs = ["chars"] }


fileStyleTests :: Invocation -> Test
fileStyleTests i = "file style" ~:
    [ "line input" ~: utf8Test iFile
        [utf8 "xαβyψ안기q영\nquit\n"]
        [ prompt' 0, output 0 (utf8 "xαβyψ안기q영") <> prompt' 1]
    , "char input" ~: utf8Test iFileChar
        [utf8 "xαβt"]
        [ prompt' 0
        , output 0 (utf8 "x")
            <> prompt' 1 <> output 1 (utf8 "α")
            <> prompt' 2 <> output 2 (utf8 "β")
            <> prompt' 3 <> output 3 (utf8 "t")
            <> prompt' 4]
    , "invalid line input" ~: utf8Test iFile
        -- NOTE: the 206 is an incomplete byte sequence,
        -- but we MUST not pause since we're at EOF, not just
        -- end of term.
        --
        -- Also recall GHC bug #5436 which caused a crash
        -- if the last byte started an incomplete sequence.
        [ utf8 "a" <> raw [149] <> utf8 "x" <> raw [206] ]
        [ prompt' 0
        , B.empty
        -- It only prompts after the EOF.
        , output 0 (utf8 "a" <> err <> utf8 "x" <> err) <> prompt' 1
        ]
    , "invalid char input (following a newline)" ~: utf8Test iFileChar
        [ utf8 "a\n" <> raw [149] <> utf8 "x\n" <> raw [206] ]
        $ [ prompt' 0
          , output 0 (utf8 "a")
             <> prompt' 1 <> output 1 err
             <> prompt' 2 <> output 2 (utf8 "x")
             <> prompt' 3
             <> whenLegacy (output 3 err <> prompt' 4)
          ] ++ if legacyEncoding then [] else [ output 3 err <> prompt' 4 ]
    , "invalid char file input (no preceding newline)" ~: utf8Test iFileChar
        [ utf8 "a" <> raw [149] <> utf8 "x" <> raw [206] ]
            -- make sure it tries to read a newline
            -- and instead gets the incomplete 206.
            -- This should *not* cause it to crash or block.
        $ [ prompt' 0
          , output 0 (utf8 "a")
             <> prompt' 1 <> output 1 err
             <> prompt' 2 <> output 2 (utf8 "x")
             <> prompt' 3
             <> whenLegacy (output 3 err <> prompt' 4)
          ] ++ if legacyEncoding then [] else [ output 3 err <> prompt' 4 ]
     ]
    -- also single char and buffer break and other stuff
  where
    iFile = i { runInTTY = False }
    iFileChar = setCharInput iFile

-- Test that the dumb terminal backend does encoding correctly.
-- If all the above tests work for the terminfo backend,
-- then we just need to make sure the dumb term plugs into everything
-- correctly, i.e., encodes the input/output and doesn't double-encode.
dumbTests :: Invocation -> Test
dumbTests i = "dumb term" ~:
    [ "line input" ~: utf8Test i
        [ utf8 "xαβγy" ]
        [ prompt' 0, utf8 "xαβγy" ]
    , "line input wide movement" ~: utf8Test i
        [ utf8 wideChar, raw [1], raw [5] ]
        [ prompt' 0, utf8 wideChar
        , utf8 (T.replicate 60 "\b")
        , utf8 wideChar
        ]
    , "line char input" ~: utf8Test (setCharInput i)
        [utf8 "xαβ"]
        [ prompt' 0, utf8 "x" <> nl <> output 0 (utf8 "x")
          <> prompt' 1 <> utf8 "α" <> nl <> output 1 (utf8 "α")
          <> prompt' 2 <> utf8 "β" <> nl <> output 2 (utf8 "β")
          <> prompt' 3
        ]
    ]
  where
    wideChar = T.concat $ replicate 10 $ "안기영"

-------------
-- Building blocks for expected input/output

smkx,rmkx :: B.ByteString
smkx = utf8 "\ESC[?1h\ESC="
rmkx = utf8 "\ESC[?1l\ESC>"

prompt, prompt' :: Int -> B.ByteString
-- the terminfo backend has the terminal use the kitty keyboard protocol
-- while it reads (this fork, see FORK.md)
prompt k = smkx <> pushProtocol <> prompt' k

prompt' k = utf8 (T.pack (show k ++ ":"))

end :: B.ByteString
end = nl <> popProtocol <> rmkx

pushProtocol, popProtocol :: B.ByteString
pushProtocol = utf8 "\ESC[>1u"
popProtocol = utf8 "\ESC[<u"

cr :: B.ByteString
cr = raw [13]

nl :: B.ByteString
nl = raw [13,10] -- NB: see fixNL: this is really [13,13,10]

output :: Int -> B.ByteString -> B.ByteString
output k s = utf8 (T.pack $ "line " ++ show k ++ ":")
                <> s <> raw [10]

wrap :: B.ByteString
wrap = utf8 " \b"

utf8 :: T.Text -> B.ByteString
utf8 = E.encodeUtf8

raw :: [Word8] -> B.ByteString
raw = B.pack

err :: B.ByteString
err = if legacyEncoding
                then utf8 "?"
                else utf8 "\65533"

----------------------

utf8Test ::
  Invocation -> [BC.ByteString] -> [BC.ByteString] -> Test
utf8Test = testI . setUTF8

utf8TestInvalidHist ::
  Invocation -> [BC.ByteString] -> [BC.ByteString] -> Test
utf8TestInvalidHist i input output' = test $ do
    B.writeFile "myhist" $ invalidHist
    assertInvocation (setUTF8 i) input output'

utf8TestValidHist ::
  Invocation -> [BC.ByteString] -> [BC.ByteString] -> Test
utf8TestValidHist i input output' = test $ do
    B.writeFile "myhist" validHist
    assertInvocation (setUTF8 i) input output'

latin1TestInvalidHist ::
  Invocation -> [BC.ByteString] -> [BC.ByteString] -> Test
latin1TestInvalidHist i input output' = test $ do
    B.writeFile "myhist" $ invalidHist
    assertInvocation (setLatin1 i) input output'

-------------
-- Tests of what this fork adds (see FORK.md)

forkTests :: FilePath -> Invocation -> Test
forkTests cwd i = "fork" ~: test
    [ multilineTests cwd (setUTF8 i)
    , multilineHistoryTests (setUTF8 i)
    , editorTests cwd (setUTF8 i)
    , keyboardProtocolTests cwd (setUTF8 i)
    ]

shiftReturn :: B.ByteString
shiftReturn = utf8 "\ESC[13;2u"

-- What drawing a newline in the line writes: the rest of the row is
-- cleared and the next line is indented by the width of the prompt ("0:").
newline :: B.ByteString
newline = clearEOL <> cr <> nl <> utf8 "  "

-- To the start of the row above.
up1 :: B.ByteString
up1 = cr <> utf8 "\ESC[A"

clearEOL :: B.ByteString
clearEOL = utf8 "\ESC[K"

withHome :: FilePath -> String -> Invocation -> Invocation
withHome cwd home = setEnv "HOME" (cwd ++ "/tests/homes/" ++ home)

multilineTests :: FilePath -> Invocation -> Test
multilineTests cwd i = "multiline" ~:
    [ "shift-return (CSI u) inserts a newline" ~: testI i
        ["a", shiftReturn, "b", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , end <> output 0 (utf8 "a\nb") <> prompt 1 ]
    , "the next lines are indented by the width of the prompt" ~: testI i
        (replicate 10 "\n" ++ ["a", shiftReturn, "b"])
        ( prompt 0
        : [end <> output k "" <> prompt (k + 1) | k <- [0 .. 9]]
        ++ [utf8 "a", clearEOL <> cr <> nl <> utf8 "   ", utf8 "b"] )
    , "shift-return (modifyOtherKeys)" ~: testI i
        ["a", utf8 "\ESC[27;2;13~", "b", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , end <> output 0 (utf8 "a\nb") <> prompt 1 ]
    , "meta-return" ~: testI i
        ["a", utf8 "\ESC\r", "b", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , end <> output 0 (utf8 "a\nb") <> prompt 1 ]
    , "shift-return from a keyseq in .haskeline" ~:
        testI (withHome cwd "keyseq" i)
        ["a", utf8 "\ESC[99~", "b", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , end <> output 0 (utf8 "a\nb") <> prompt 1 ]
    , "inserting in the first line redraws the lines after it" ~: testI i
        ["a", shiftReturn, "b", "\ESC[A", "x"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , up1 <> utf8 "\ESC[3C"
        , utf8 "x" <> newline <> utf8 "b" <> up1 <> utf8 "\ESC[4C" ]
    , "accepting from the first line moves below the last" ~: testI i
        ["a", shiftReturn, "b", "\ESC[A", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , up1 <> utf8 "\ESC[3C"
        , nl <> end <> output 0 (utf8 "a\nb") <> prompt 1 ]
    , "deleting a newline joins the lines and clears the row" ~: testI i
        ["a", shiftReturn, "b", "\DEL", "\DEL", "c", "\n"]
        [ prompt 0, utf8 "a", newline, utf8 "b"
        , utf8 "\b" <> clearEOL
        , up1 <> utf8 "\ESC[3C" <> clearEOL <> nl <> clearEOL
            <> up1 <> utf8 "\ESC[3C"
        , utf8 "c"
        , end <> output 0 (utf8 "ac") <> prompt 1 ]
    , "up and down keep the column as far as the line is long" ~: testI i
        ["abc", shiftReturn, "d", "\ESC[A", "\ESC[B"]
        [ prompt 0, utf8 "abc", newline, utf8 "d"
        , up1 <> utf8 "\ESC[3C"
        , cr <> nl <> utf8 "\ESC[3C" ]
    , "vi: j and k move between lines, onto the last character"
        ~: testI vi
        ["abc", shiftReturn, "de", "\ESC", "k", "j"]
        [ prompt 0, utf8 "abc", newline, utf8 "de"
        , utf8 "\b"
        , up1 <> utf8 "\ESC[3C"
        , cr <> nl <> utf8 "\ESC[3C" ]
    , "vi: k on a shorter line goes on its last character" ~: testI vi
        ["a", shiftReturn, "bcd", "\ESC", "k"]
        [ prompt 0, utf8 "a", newline, utf8 "bcd"
        , utf8 "\b"
        , up1 <> utf8 "\ESC[2C" ]
    , "vi: escape and return arriving together leave insert mode and accept"
        ~: testI (withHome cwd "vi-noprotocol" i) ["abc", "\ESC\r"]
        [ smkx <> prompt' 0, utf8 "abc"
        , utf8 "\b" <> nl <> rmkx <> output 0 (utf8 "abc") <> smkx
            <> prompt' 1 ]
    , "vi: k on the first line goes back in history" ~: test $ do
        B.writeFile "myhist" B.empty
        assertInvocation vi
            [ "a", shiftReturn, "b", "\n"
            , "x", shiftReturn, "y", "\ESC", "k", "k" ]
            [ prompt 0, utf8 "a", newline, utf8 "b"
            , end <> output 0 (utf8 "a\nb") <> prompt 1
            , utf8 "x", newline, utf8 "y", utf8 "\b"
            , up1 <> utf8 "\ESC[2C"
            -- history puts the cursor at the start in vi mode
            , utf8 "a" <> newline <> utf8 "b" <> up1 <> utf8 "\ESC[2C" ]
    ]
  where
    vi = withHome cwd "vi" i

multilineHistoryTests :: Invocation -> Test
multilineHistoryTests i = "multiline history" ~:
    [ "entries are written escaped" ~: test $ do
        writeHistoryFile B.empty
        _ <- runInvocation i
            [ "a", shiftReturn, utf8 "b\\c", "\n", "quit\n" ]
        written <- readHistoryFile
        written @?= escapedHeader
                    <> utf8 "quit\na\\nb\\\\c\n"
    , "escaped entries are read back" ~: test $ do
        writeHistoryFile $ escapedHeader <> utf8 "x\\ny\\\\z\n"
        assertInvocation i [ "\ESC[A" ]
            [ prompt 0, utf8 "x" <> newline <> utf8 "y\\z" ]
    , "a file without the header is read as it is" ~: test $ do
        writeHistoryFile $ utf8 "x\\ny\n"
        assertInvocation i [ "\ESC[A" ]
            [ prompt 0, utf8 "x\\ny" ]
    ]
  where
    escapedHeader = utf8 "# haskeline history: escaped entries\n"
    writeHistoryFile = B.writeFile "myhist"
    -- the history is written as the program exits, which may be a moment
    -- after the last output
    readHistoryFile = loop (50 :: Int)
      where
        loop n = do
            contents <- B.readFile "myhist"
            if B.null contents && n > 0
                then threadDelay 20000 >> loop (n - 1)
                else return contents

editorTests :: FilePath -> Invocation -> Test
editorTests cwd i = "ctrl-g opens the line in EDITOR" ~:
    [ "what the editor saves becomes the line, newlines kept"
        ~: testI (editor "append-line") ["one", ctrlG, "!", "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> newline <> utf8 "added", utf8 "!"
        , end <> output 0 (utf8 "one\nadded!") <> prompt 1 ]
    , "EDITOR can have arguments"
        ~: testI (editor "append-args --flag") ["one", ctrlG, "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> utf8 " --flag"
        , end <> output 0 (utf8 "one --flag") <> prompt 1 ]
    , "ctrl-c while the editor runs ends the editor and not the program"
        ~: testI (editor "wait") ["one", ctrlG, raw [3], "\n"]
        -- the terminal echoes ^C while the editor has it
        [ prompt 0, utf8 "one", popProtocol
        , utf8 "^C" <> pushProtocol <> bell
        , end <> output 0 (utf8 "one") <> prompt 1 ]
    , "VISUAL before EDITOR"
        ~: testI (setEnv "VISUAL" (editors ++ "/append-args --visual")
                    (editor "fail"))
            ["one", ctrlG, "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> utf8 " --visual"
        , end <> output 0 (utf8 "one --visual") <> prompt 1 ]
    , "undo goes back to the line before"
        ~: testI (editor "append-line") ["one", ctrlG, raw [31], "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> newline <> utf8 "added"
        , up1 <> utf8 "\ESC[5C" <> clearEOL <> nl <> clearEOL <> up1
            <> utf8 "\ESC[5C"
        , end <> output 0 (utf8 "one") <> prompt 1 ]
    , "an editor that fails leaves the line and rings the bell"
        ~: testI (editor "fail") ["one", ctrlG, "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> bell
        , end <> output 0 (utf8 "one") <> prompt 1 ]
    , "vim when EDITOR is not set"
        ~: testI (setEnv "PATH" editors slow) ["one", ctrlG, "\n"]
        [ prompt 0, utf8 "one", aroundEditor <> utf8 " (vim)"
        , end <> output 0 (utf8 "one (vim)") <> prompt 1 ]
    , "vi: in command mode too"
        ~: testI (withHome cwd "vi" (editor "append-line"))
            ["one", "\ESC", ctrlG, "\n"]
        [ prompt 0, utf8 "one", utf8 "\b"
        -- the cursor goes on the last character, after the 'e' it was on
        , aroundEditor <> utf8 "e" <> newline <> utf8 "added\b"
        , end <> output 0 (utf8 "one\nadded") <> prompt 1 ]
    ]
  where
    editors = cwd ++ "/tests/editors"
    editor e = setEnv "EDITOR" (editors ++ "/" ++ e) slow
    -- the editor is a shell script that takes a moment to run
    slow = i { pause = 300000 }
    ctrlG = raw [7]
    bell = raw [7]
    -- the editor runs with the terminal's keyboard mode as it was
    aroundEditor = popProtocol <> pushProtocol

-- The terminal sends these keys this way once haskeline has it use the kitty
-- keyboard protocol; here they are sent as a terminal with it would.
keyboardProtocolTests :: FilePath -> Invocation -> Test
keyboardProtocolTests cwd i = "kitty keyboard protocol" ~:
    [ "vi: escape followed at once by a key is escape and the key"
        ~: testI vi ["ab", utf8 "\ESC[27u0", "x", "\n"]
        [ prompt 0, utf8 "ab", left 2, utf8 "b" <> clearEOL <> utf8 "\b"
        , end <> output 0 (utf8 "b") <> prompt 1 ]
    , "ctrl and a letter" ~: testI i ["abc", utf8 "\ESC[97;5u", "x", "\n"]
        [ prompt 0, utf8 "abc", left 3, utf8 "xabc" <> left 3
        , end <> output 0 (utf8 "xabc") <> prompt 1 ]
    , "alt and a letter" ~: testI i ["ab cd", utf8 "\ESC[98;3u", "x", "\n"]
        [ prompt 0, utf8 "ab cd", left 2, utf8 "xcd" <> left 2
        , end <> output 0 (utf8 "ab xcd") <> prompt 1 ]
    , "the terminal's kill character" ~:
        testI i ["abc", utf8 "\ESC[117;5u", "x", "\n"]
        [ prompt 0, utf8 "abc", left 3 <> clearEOL, utf8 "x"
        , end <> output 0 (utf8 "x") <> prompt 1 ]
    , "ctrl-c interrupts" ~: testI i ["ab", utf8 "\ESC[99;5u"]
        [ prompt 0, utf8 "ab"
        , end <> output 0 (utf8 "Caught interrupted") <> prompt 1 ]
    , "ctrl-z stops the program with the protocol off"
        ~: testI i ["ab", utf8 "\ESC[122;5ucd", "e\n"]
        [ prompt 0, utf8 "ab", popProtocol, B.empty ]
    , "keypad enter" ~: testI i ["ab", utf8 "\ESC[57414u"]
        [ prompt 0, utf8 "ab", end <> output 0 (utf8 "ab") <> prompt 1 ]
    , "home" ~: testI i ["ab", utf8 "\ESC[H", "x", "\n"]
        [ prompt 0, utf8 "ab", left 2, utf8 "xab" <> left 2
        , end <> output 0 (utf8 "xab") <> prompt 1 ]
    , "function keys are keys (here unbound)"
        ~: testI i ["ab", utf8 "\ESC[13~", "c", "\n"]
        [ prompt 0, utf8 "ab", bell, utf8 "c"
        , end <> output 0 (utf8 "abc") <> prompt 1 ]
    , "replies from the terminal are ignored"
        ~: testI i ["ab", utf8 "\ESC[?1u\ESC[?62;22c", "c", "\n"]
        [ prompt 0, utf8 "ab", B.empty, utf8 "c"
        , end <> output 0 (utf8 "abc") <> prompt 1 ]
    , "not used when .haskeline says not to"
        ~: testI (withHome cwd "noprotocol" i) ["ab\n"]
        [ smkx <> prompt' 0
        , utf8 "ab" <> nl <> rmkx <> output 0 (utf8 "ab")
            <> smkx <> prompt' 1 ]
    ]
  where
    vi = withHome cwd "vi" i
    left n = utf8 (T.pack ("\ESC[" ++ show (n :: Int) ++ "D"))
    bell = raw [7]
