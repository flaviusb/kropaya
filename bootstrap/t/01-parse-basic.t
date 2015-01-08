#!/bin/bash

. $(dirname $0)/test.sh

plan 4

name "Parse an int"
first=`echo "3" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVAtomicValue (IntValue 3))))]"
expect_eq "$first" "$first_expect"

name "Parse a square string"
first=`echo "#[foo bar baz]" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVAtomicValue (TextValue \"foo bar baz\"))))]"
expect_eq "$first" "$first_expect"

name "Parse an interpolated quoted string"
first=`echo "\"foo #{#[235 2435 h]} bar baz\"" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVAtomicValue (InterpolatedTextValue [Right \"foo \",Left [JustExpression (Expression [] (Right (CVAtomicValue (TextValue \"235 2435 h\"))))],Right \" bar baz\"]))))]"
expect_eq "$first" "$first_expect"

name "Parse a literal label"
first=`echo "&zog" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVLabelLit (LabelLit \"zog\"))))]"
expect_eq "$first" "$first_expect"
