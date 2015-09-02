#!/bin/bash

. $(dirname $0)/test.sh

plan 1

emacs --batch -f batch-byte-compile ../bootstrap-kropaya-interpreter.el
runemacs='emacs --batch -l ../bootstrap-kropaya-interpreter.el'

name "Test parsing some tiny programs."
to_todo "Finish juxtaposition parser."
first=`$runemacs -l 02-parse-complete-small-program-0.el`
first_expect=`cat 02-parse-complete-small-program-0.out`
echo "$first" > smoo1
echo "$first_expect" > smoo2
expect_eq "$first" "$first_expect"

