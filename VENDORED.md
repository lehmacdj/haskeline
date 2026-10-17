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
  and in Vi insert mode. A terminal sends shift-return as return unless it is
  set up to tell them apart, so the patch reads the two encodings terminals
  use: `ESC [ 13 ; 2 u` (CSI u, which the kitty keyboard protocol and most
  "send this text for shift+enter" settings use) and `ESC [ 27 ; 2 ; 13 ~`
  (xterm's modifyOtherKeys). Meta-return (escape and return arriving
  together) also inserts a newline, for terminals that can send that but
  nothing else. A terminal that sends something else for shift-return can
  have it read in `~/.haskeline`, for example
  `keyseq: "\ESC[99~" shift-return` (`parseKey` used to read
  `shift-return` as return).
  `Command.hs` (`newlineCmds`), `Backend/Posix.hsc` (`ansiKeys`), `Key.hs`.
- The line is drawn with its newlines: a newline clears the rest of its row
  and the line goes on at the start of the next. The cursor is moved to
  where drawing the line up to it from the prompt would leave it, instead of
  by a number of cells, which a newline makes wrong.
  `Backend/Terminfo.hs`. The dumb terminal backend (`TERM=dumb`) is not
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
Ctrl-G tests run a shell script as the editor).

Two upstream tests fail on macOS, before the patch and after it: "tab
completion" needs `tests/dummy-μασ/`, which the Hackage tarball does not
have, and "history encoding" 2 needs the `en_US.ISO8859-1` locale.
