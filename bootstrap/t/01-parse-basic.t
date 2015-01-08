#!/bin/bash

. $(dirname $0)/test.sh

plan 18

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

name "Parse a variable"
first=`echo zog | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVVariable (Variable \"zog\"))))]"
expect_eq "$first" "$first_expect"

name "Parse Integer type"
first=`echo Integer | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTAtomicType IntType)))]"
expect_eq "$first" "$first_expect"

name "Parse Decimal type"
first=`echo Decimal | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTAtomicType DecimalType)))]"
expect_eq "$first" "$first_expect"

name "Parse Text type"
first=`echo Text | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTAtomicType TextType)))]"
expect_eq "$first" "$first_expect"

name "Parse Binary type"
first=`echo Binary | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTAtomicType BinaryType)))]"
expect_eq "$first" "$first_expect"

name "Parse Symbol type"
first=`echo Symbol | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTAtomicType SymbolType)))]"
expect_eq "$first" "$first_expect"

name "Parse simple row type"
first=`echo "⦇&foo::Integer⦈" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTRowType (RowType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple row type with whitespace"
first=`echo "⦇ &foo :: Integer ⦈" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTRowType (RowType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple product type"
first=`echo "{&foo::Integer}" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTProductType (ProductType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple product type with whitespace"
first=`echo "{ &foo :: Integer }" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTProductType (ProductType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple sum type"
first=`echo "<&foo::Integer>" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple sum type with whitespace"
first=`echo "< &foo :: Integer >" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple product value"
first=`echo "{&three ⇒ 3}" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVProductValue (ProductValue [LabelSectionValue (LabelLitBit (LabelLit \"three\")) (Expression [] (Right (CVAtomicValue (IntValue 3))))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple sum value"
first=`echo "<&three ⇒ 3>" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Right (CVSumValue (SumValue [LabelSectionValue (LabelLitBit (LabelLit \"three\")) (Expression [] (Right (CVAtomicValue (IntValue 3))))]))))]"
expect_eq "$first" "$first_expect"
