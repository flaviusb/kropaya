Kropaya Bootstrap Interpreter
=============================

This is a elisp program that interprets a limited form of the Kropaya language, complete with a compiler for this form of the language written in this limited form of Kropaya.

Build a bootstrap compiler using the provided `mk.sh`, or by running

~~~
emacs --batch --script bootstrap-kropaya-interpreter.el bootstrap-kropaya-compiler.kropaya bootstrap-kropaya-compiler-1
./bootstrap-kropaya-compiler-1 bootstrap-kropaya-compiler.kropaya bootstrap-kropaya-compiler
~~~

Tests are in the `t` directory, though at the moment they are entirely misleading and in fact will not work at all.
