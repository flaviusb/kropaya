#!/bin/bash

. $(dirname $0)/test.sh

plan 2

name "Parse an int"
first=`echo "3" | ../dist/build/kropaya-bootstrap/kropaya-bootstrap`
first_expect="Right [KR0Type (KAtomic (KInt 3)),NL]"
expect_eq "$first" "$first_expect"

name "Parse a square string"
first=`echo "#[foo bar baz]" | ../dist/build/kropaya-bootstrap/kropaya-bootstrap`
first_expect="Right [KR0Type (KAtomic (KSSt \"foo bar baz\")),NL]"
expect_eq "$first" "$first_expect"

