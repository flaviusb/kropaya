#!/bin/bash

. $(dirname $0)/test.sh

plan 5

emacs --batch -f batch-byte-compile ../bootstrap-kropaya-interpreter.el
runemacs='emacs --batch -l ../bootstrap-kropaya-interpreter.el'

name "Test super basic parser combinators"
first=`$runemacs -l 01-parser-check-0.el`
first_expect=`cat 01-parser-check-0.out`
#echo "$first" > goo1
#echo "$first_expect" > goo2
expect_eq "$first" "$first_expect"

name "Test string parser combinators."
first=`$runemacs -l 01-parser-check-1.el`
first_expect=`cat 01-parser-check-1.out`
#echo "$first" > foo1
#echo "$first_expect" > foo2
expect_eq "$first" "$first_expect"

name "Test int, number parser combinators."
first=`$runemacs -l 01-parser-check-2.el`
first_expect=`cat 01-parser-check-2.out`
#echo "$first"
#echo "$first_expect"
expect_eq "$first" "$first_expect"

name "Test identifier, ws, comment parser combinators."
first=`$runemacs -l 01-parser-check-3.el`
first_expect=`cat 01-parser-check-3.out`
#echo "$first" > moo1
#echo "$first_expect" > moo2
expect_eq "$first" "$first_expect"

name "Test quantifier parser."
first=`$runemacs -l 01-parser-check-4.el`
first_expect=`cat 01-parser-check-4.out`
#echo "$first" > moo1
#echo "$first_expect" > moo2
expect_eq "$first" "$first_expect"

