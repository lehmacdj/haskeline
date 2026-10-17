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
  at its end; trailing newlines are dropped and others become spaces. When
  the editor fails, the line stays and the bell rings.
  `System/Console/Haskeline/Command/Editor.hs`, bound in `Emacs.hs` and
  `Vi.hs`.
