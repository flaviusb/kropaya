#Project Layout

The bootstrap interpreter is written in Emacs Lisp, and lives in the `bootstrap` directory.

Tests are in `t` subdirectories of each code directory, and are TAP speaking shell scripts; use any equivalent of `prove` to run them.

The compiler is envisaged to be written in a multi-stage manner. Each stage will live in its own directory.

Typeset diagrams live in the `latex` directory.
