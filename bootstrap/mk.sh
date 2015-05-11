#!/bin/bash

emacs --batch --script bootstrap-kropaya-interpreter.el bootstrap-kropaya-compiler.kropaya bootstrap-kropaya-compiler-1
./bootstrap-kropaya-compiler-1 bootstrap-kropaya-compiler.kropaya bootstrap-kropaya-compiler
