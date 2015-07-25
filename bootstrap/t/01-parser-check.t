#!/bin/bash

. $(dirname $0)/test.sh

plan 1

emacs --batch -f batch-byte-compile ../bootstrap-kropaya-interpreter.el
runemacs='emacs --batch -l ../bootstrap-kropaya-interpreter.el'

name "Test parser combinators."
first=`$runemacs -l 01-parser-check.el`
first_expect=`cat 01-parser-check.out`
echo "$first"
echo "$first_expect"
expect_eq "$first" "$first_expect"
