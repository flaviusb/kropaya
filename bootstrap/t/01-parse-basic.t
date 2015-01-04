#!/bin/bash

. $(dirname $0)/test.sh

plan 1

name "Parse an int"
first=`echo "3" | ../dist/build/kropaya-bootstrap/kropaya-bootstrap`
first_expect="Right [KR0Type (KAtomic (KInt 3)),NL]"
if [ "$first" = "$first_expect" ]; then
  ok
else
  not_ok
fi
