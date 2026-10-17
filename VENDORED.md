# haskeline, vendored

This is haskeline 0.8.2.1 from Hackage (`stack unpack haskeline-0.8.2.1`),
used by `ge` in place of the release through `extra-deps` in
`../stack.yaml`.

It is vendored because haskeline has no way for a program to bind a key to
an action of its own. The commits that change this directory after the one
that added it are the patch; `jj diff` from that commit shows all of it.

The files keep their upstream formatting: the repository's jj config
excludes `cli/haskeline/**` from the ormolu fix tool.

## The patch

- Ctrl-G (Emacs mode, and both insert and command mode in Vi mode) opens
  the line in `EDITOR` (`vim` when it is not set), run as a shell runs
  `$EDITOR file`. What the editor saves becomes the line, with the cursor
  at its end; trailing newlines are dropped and others stay. When
  the editor fails, the line stays and the bell rings.
  `System/Console/Haskeline/Command/Editor.hs`, bound in `Emacs.hs` and
  `Vi.hs`.
- A line can have newlines in it: shift-return inserts one, in Emacs mode
  and in Vi insert mode. A terminal sends shift-return as return unless it
  uses the kitty keyboard protocol (below) or is set up to tell them apart,
  so the patch reads the two encodings terminals use: `ESC [ 13 ; 2 u` (CSI u, which the kitty keyboard protocol and most
  "send this text for shift+enter" settings use) and `ESC [ 27 ; 2 ; 13 ~`
  (xterm's modifyOtherKeys). Meta-return (escape and return arriving
  together) also inserts a newline, for terminals that can send that but
  nothing else. A terminal that sends something else for shift-return can
  have it read in `~/.haskeline`, for example
  `keyseq: "\ESC[99~" shift-return` (`parseKey` used to read
  `shift-return` as return).
  `Command.hs` (`newlineCmds`), `Backend/Posix.hsc` (`ansiKeys`), `Key.hs`.
- While a line is read (on a terminal that the terminfo backend draws on)
  the terminal is asked to use the [kitty keyboard
  protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/), its first
  enhancement only ("disambiguate escape codes"): `ESC [ > 1 u` before and
  `ESC [ < u` after. Terminals with the protocol (kitty, WezTerm, Ghostty,
  foot, Alacritty, recent iTerm2 among them) then send shift-return as
  `ESC [ 13 ; 2 u` with no setting up, and escape as a sequence of its own,
  so that escape and a key that follows at once (fast typing in Vi mode)
  are no longer read as meta and the key. Terminals without the protocol
  ignore the request. `kittyKeyboardProtocol: False` in `~/.haskeline`
  turns it off.
  - The protocol's sequences (`CSI codepoint ; modifiers u`, and
    `CSI 1 ; modifiers letter` and `CSI number ; modifiers ~` for keys such
    as the arrows) are read as the keys a terminal without it would send:
    ctrl and a letter is a control character, alt a meta key, the keypad
    keys are the keys they stand for, and a control character is read
    through the terminal's erase and kill characters and `~/.haskeline`'s
    sequences as the typed one is. A key with super or hyper is ignored, as
    is a control sequence that isn't a key (a reply from the terminal) or
    one haskeline has no key for.
  - With the protocol the terminal sends ctrl-c, ctrl-\ and ctrl-z as keys
    rather than as signals, so haskeline raises the signal the terminal
    would have (when its ISIG is set) and drops the rest of that input, as
    the terminal does. The protocol is off while the program is stopped
    (ctrl-z) or quits (ctrl-\), and while Ctrl-G's editor runs, so that the
    shell and the editor get keys as they expect.
  - A program that ends without haskeline's cleanup (killed, or crashed)
    leaves the terminal using the protocol; return, tab, backspace and text
    still work, and `printf '\e[<u'` (or `reset`) ends it.
  `KeyboardProtocol.hs`, used in `Backend/Posix.hsc` (`lexKeys`,
  `withKeyboardProtocol`, `raiseSignalOfKeys`), `Backend/Terminfo.hs`,
  `Prefs.hs` and `Command/Editor.hs`.
- The line is drawn with its newlines: a newline clears the rest of its row
  and the line goes on at the next, indented by the width of the prompt's
  last line so that the lines of the text start in the same column (the
  spaces are only drawn, they aren't in the text; a row that wraps isn't
  indented). The cursor is moved to where drawing the line up to it from
  the prompt would leave it, instead of by a number of cells, which a
  newline makes wrong. `Backend/Terminfo.hs`, `RunCommand.hs`
  (`indentLines`). The dumb terminal backend (`TERM=dumb`) is not
  changed and draws a newline as it is.
- Up and down (and ctrl-p and ctrl-n in Emacs mode, j and k in Vi command
  mode) move to the line above or below, keeping the column, and go through
  the history only from the first or last line. In Vi command mode the
  cursor goes on the last character of a shorter line. `LineState.hs`
  (`lineUp`, `lineDown`), `Command/History.hs` (`lineUpOr`, `lineDownOr`),
  `Vi.hs`. Other motions (`0`, `$`, Home, End, ctrl-a, ctrl-e) still go to
  the start or end of the whole line.
- The history file can hold entries with newlines: it starts with the line
  `# haskeline history: escaped entries`, and its entries are escaped
  (`\\` and `\n`). A file without that line is read as before, one entry
  per line. An unpatched haskeline reads an escaped file with the escapes in
  its entries. `History.hs`.

## Tests

`./run-tests.sh` runs haskeline's tests (stack does not run the tests of an
extra-dep); the patch's are `vendoredTests` in `tests/Unit.hs`. They drive
`haskeline-examples-Test` in a pseudoterminal and compare what it writes.
The patch also changes the test harness: each test sets `HOME` to a
directory in `tests/homes/` (without it haskeline found the `~/.haskeline`
of whoever ran the tests, and several upstream tests failed in Vi mode), and
`RunTTY`'s `pause` sets how long to wait for output after each input (the
Ctrl-G tests run a shell script as the editor), and the output of an input
is read until there is no more for that long (the program may write it in
several writes).

Two upstream tests failed before the patch. "Tab completion" needs
`tests/dummy-μασ/` (with the files `ςερτ` and `bar`), which the Hackage
tarball does not have; the copy has it now. The third "history encoding"
test, which reads the history in latin-1, is skipped on macOS: GHC's locale
encoding there is UTF-8 whatever `LANG` says, so it can't pass.
