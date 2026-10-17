# This fork of haskeline

This is haskeline 0.8.2.1 (the `0.8.2.1` tag, with Hackage's revision 3 of
`haskeline.cabal`) and the changes below, made for `ge`, the graph editor
of [lehmacdj/graph](https://github.com/lehmacdj/graph), which uses it
through a `git` entry in the `extra-deps` of its `stack.yaml`. They live
here because haskeline has no way for a program to bind a key to an action
of its own. The commits after the tag are the changes, one each.

Its version is 0.8.2.1.1: with the release's version stack takes the
haskeline that comes with GHC 9.6 for it and never builds the fork.

The files keep their upstream formatting; the repository's jj config turns
the ormolu fix tool off.

## The changes

- Ctrl-G (Emacs mode, and both insert and command mode in Vi mode) opens
  the line in `VISUAL`, or `EDITOR` (`vim` when neither is set), run as a
  shell runs `$EDITOR file`. What the editor saves becomes the line, with
  the cursor at its end; trailing newlines are dropped and others stay.
  When the editor fails, the line stays and the bell rings.
  While the editor runs, ctrl-c and ctrl-\ go to the editor only (an
  editor they end has failed): the program lets SIGINT and SIGQUIT go by,
  with `sigaction` (`cbits/h_signals.c`), restoring exactly the actions it
  had; not with `process`'s `delegate_ctlc`, which restores the handler
  that the GHC runtime knows, the wrong one for a program that installed
  its own without it (as ge does), and rethrows an interrupt that ended the
  editor. The editor's output isn't cleared from the screen: an editor that
  leaves some there (`code --wait`'s message) leaves the line drawn wrong
  until it is redrawn (ctrl-l).
  Undo goes back to the line before Ctrl-G.
  A failed editor leaves nothing to undo.
  `System/Console/Haskeline/Command/Editor.hs`, bound in `Emacs.hs` and
  `Vi.hs`.
- A line can have newlines in it: shift-return inserts one, in Emacs mode
  and in Vi insert mode. A terminal sends shift-return as return unless it
  uses the kitty keyboard protocol (below) or is set up to tell them apart,
  so the fork reads the two encodings terminals use: `ESC [ 13 ; 2 u`
  (CSI u, which the kitty keyboard protocol and most
  "send this text for shift+enter" settings use) and `ESC [ 27 ; 2 ; 13 ~`
  (xterm's modifyOtherKeys). In Emacs mode meta-return (escape and return
  arriving together) also inserts a newline, for terminals that can send
  that but nothing else; in Vi insert mode it is escape and return, typed
  quickly, and leaves insert mode and accepts the line. A terminal that sends something else for shift-return can
  have it read in `~/.haskeline`, for example
  `keyseq: "\ESC[99~" shift-return` (`parseKey` used to read
  `shift-return` as return).
  `Command.hs` (`newlineCmds`), `Backend/Posix.hsc` (`ansiKeys`), `Key.hs`.
- While a line is read (on a terminal that the terminfo backend draws on)
  the terminal is asked to use the [kitty keyboard
  protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/), with the
  enhancements "disambiguate escape codes" and "report alternate keys":
  `ESC [ > 5 u` before and `ESC [ < u` after. Terminals with the protocol (kitty, WezTerm, Ghostty,
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
    sequences as the typed one is. A key with shift is read as the
    character shift makes of it (from the alternate keys: alt and shift and
    `,` is meta-`<`, ctrl and shift and `-` is ctrl-`_`), and a key with
    ctrl or alt on a layout other than the US one as the US layout's key in
    its place (ctrl and Cyrillic `с` is ctrl-c), as a terminal without the
    protocol sends them. A key with super or hyper is ignored, as is a
    control sequence that isn't a key (a reply from the terminal) or one
    haskeline has no key for. When the input ends partway through a control
    sequence (it can come in two reads, over ssh say), haskeline waits up
    to 100ms for the rest.
    It waits too when the input ends in an escape, once the terminal has
    sent the escape key the protocol's way (`CSI 27 u`): an escape is then
    never a key on its own. Until then it may be the escape key of a
    terminal that ignored the request (and an escape key that waited
    would be slow in Vi mode); a modified escape (`CSI 27 ; 5 u`) is no
    sign, since terminals with a CSI u mode of their own send that too.
    An escape typed while no line is read (during a command) arrives
    without the protocol, and one typed at once after it can then be read
    with it as meta and the key, as without the protocol.
  - With the protocol the terminal sends ctrl-c, ctrl-\ and ctrl-z as keys
    rather than as signals, so haskeline sends the signal the terminal
    would have (when its ISIG is set) to the terminal's foreground process
    group, as the terminal does (so to `tee` in `ge | tee log` too), and
    drops the rest of that input. The protocol is off while the signal is
    sent, and so while the program is stopped (ctrl-z) or if it ends of
    it, and while Ctrl-G's editor runs, so that the shell and the editor
    get keys as they expect.
    After ctrl-z haskeline waits a moment (100ms) before it turns the
    protocol back on: another thread of the program (ge reads its lines on
    one) can run on for a moment before the stop, and would turn it on for
    the shell. Stopped, the program turns it on once continued.
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
  newline makes wrong. A newline after a row that the text fills exactly
  (which already went on to the next row) doesn't leave an empty row.
  `Backend/Terminfo.hs`, `RunCommand.hs`
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
  per line. Upstream haskeline reads an escaped file with the escapes in
  its entries. `History.hs`.

## Tests

`stack test` runs haskeline's tests; the fork's are `forkTests` in
`tests/Unit.hs`. They drive `haskeline-examples-Test` in a pseudoterminal
and compare what it writes. The fork also changes the test harness: each
test sets `HOME` to a directory in `tests/homes/` (without it haskeline
found the `~/.haskeline` of whoever ran the tests, and several upstream
tests failed in Vi mode), `RunTTY`'s `pause` sets how long to wait for
output after each input (the Ctrl-G tests run a shell script as the
editor), and the output of an input is read until there is no more for
that long (the program may write it in several writes).
`haskeline-examples-Test` is built `-threaded`, as ge is, and with the
argument `forked` reads its lines on a thread of its own, as ge does.
With `unheld` it catches ctrl-c with a handler that the GHC runtime
doesn't know, as ge does, instead of with `withInterrupt`.

The third "history encoding" test, which reads the history in latin-1, is
skipped on macOS: GHC's locale encoding there is UTF-8 whatever `LANG`
says, so it can't pass.
