#!/bin/bash

. $(dirname $0)/test.sh

plan 28

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
first_expect='Right [JustExpression (Expression [] (Right (CVAtomicValue (InterpolatedTextValue [Right "foo ",Left [JustExpression (Expression [] (Right (CVAtomicValue (TextValue "235 2435 h"))))],Right " bar baz"]))))]'
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

name "Parse simple plural product type"
first=`echo "{&foo::Integer, &bar::Integer,&baz :: Text ,quuz:: Decimal}" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTProductType (ProductType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType))),LabelSectionType (LabelLitBit (LabelLit \"bar\")) (Expression [] (Left (CTAtomicType IntType))),LabelSectionType (LabelLitBit (LabelLit \"baz\")) (Expression [] (Left (CTAtomicType TextType))),LabelSectionType (LabelVarBit (Variable \"quuz\")) (Expression [] (Left (CTAtomicType DecimalType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple product type with whitespace"
first=`echo "{ &foo :: Integer }" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTProductType (ProductType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple sum type"
first=`echo "<&foo::Integer>" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple plural sum type"
first=`echo "<&foo::Integer, &bar::Integer,&baz :: Text ,quuz:: Decimal>" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType))),LabelSectionType (LabelLitBit (LabelLit \"bar\")) (Expression [] (Left (CTAtomicType IntType))),LabelSectionType (LabelLitBit (LabelLit \"baz\")) (Expression [] (Left (CTAtomicType TextType))),LabelSectionType (LabelVarBit (Variable \"quuz\")) (Expression [] (Left (CTAtomicType DecimalType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple sum type with whitespace"
first=`echo "< &foo :: Integer >" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect="Right [JustExpression (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit \"foo\")) (Expression [] (Left (CTAtomicType IntType)))]))))]"
expect_eq "$first" "$first_expect"

name "Parse simple product value"
first=`echo "{&three ⇒ 3}" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVProductValue (ProductValue [LabelSectionValue (LabelLitBit (LabelLit "three")) (Expression [] (Right (CVAtomicValue (IntValue 3))))]))))]'
expect_eq "$first" "$first_expect"

name "Parse simple sum value"
first=`echo "<&three ⇒ 3>" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVSumValue (SumValue (LabelSectionValue (LabelLitBit (LabelLit "three")) (Expression [] (Right (CVAtomicValue (IntValue 3)))))))))]'
expect_eq "$first" "$first_expect"

name "Parse simple lambda type"
first=`echo "<x::y> → {x::y→a} → a" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Left (CTLambdaType (LambdaType [Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelVarBit (Variable "x")) (Expression [] (Right (CVVariable (Variable "y"))))]))),Expression [] (Left (CTProductType (ProductType [LabelSectionType (LabelVarBit (Variable "x")) (Expression [] (Left (CTLambdaType (LambdaType [Expression [] (Right (CVVariable (Variable "y"))),Expression [] (Right (CVVariable (Variable "a")))]))))]))),Expression [] (Right (CVVariable (Variable "a")))]))))]'
expect_eq "$first" "$first_expect"

name "Parse simple lambda value"
first=`echo '\a::Integer, b::Boolean⇒a' | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVLambdaValue (LambdaValue [Binding (Variable "a") (Expression [] (Left (CTAtomicType IntType))),Binding (Variable "b") (Expression [] (Left (CTAtomicType BooleanType)))] [JustExpression (Expression [] (Right (CVVariable (Variable "a"))))]))))]'
expect_eq "$first" "$first_expect"

name "Parse less simple lambda value"
first=`echo '\a::list, b::Boolean, c::<&car::Integer> ⇒ if b c' | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVLambdaValue (LambdaValue [Binding (Variable "a") (Expression [] (Right (CVVariable (Variable "list")))),Binding (Variable "b") (Expression [] (Left (CTAtomicType BooleanType))),Binding (Variable "c") (Expression [] (Left (CTSumType (SumType [LabelSectionType (LabelLitBit (LabelLit "car")) (Expression [] (Left (CTAtomicType IntType)))]))))] [JustExpression (Expression [] (Right (CVJuxt [CVVariable (Variable "if"),CVVariable (Variable "b"),CVVariable (Variable "c")])))]))))]'
expect_eq "$first" "$first_expect"

#first=`echo '\a::Integer b::Boolean foobar::Text $$$::<&dollars::Integer, &cents::Integer> ⇒ case {&dollars::Integer ⇒ \\x::Integer ⇒ print "$#{x}", &cents::Integer ⇒ \\x::Integer ⇒ print "#{x} cents" }.' | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`

name "Parse brackets"
first=`echo '("foo bar baz ) zog")' | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVAtomicValue (InterpolatedTextValue [Right "foo bar baz ) zog"]))))]'
expect_eq "$first" "$first_expect"

name "Parse basic juxtaposition"
first=`echo foo bar | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVJuxt [CVVariable (Variable "foo"),CVVariable (Variable "bar")])))]'
expect_eq "$first" "$first_expect"

name "Parse basic binding"
first=`echo "foo = (\\bar::Integer, baz::Boolean⇒(if baz bar 3)) 5 True" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustBinding (Binding (Variable "foo") (Expression [] (Right (CVJuxt [CVLambdaValue (LambdaValue [Binding (Variable "bar") (Expression [] (Left (CTAtomicType IntType))),Binding (Variable "baz") (Expression [] (Left (CTAtomicType BooleanType)))] [JustExpression (Expression [] (Right (CVJuxt [CVVariable (Variable "if"),CVVariable (Variable "baz"),CVVariable (Variable "bar"),CVAtomicValue (IntValue 3)])))]),CVAtomicValue (IntValue 5),CVVariable (Variable "True")]))))]'
expect_eq "$first" "$first_expect"

name "Parse first brackets"
first=`echo "((6) 5) 3" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVJuxt [CVJuxt [CVAtomicValue (IntValue 6),CVAtomicValue (IntValue 5)],CVAtomicValue (IntValue 3)])))]'
expect_eq "$first" "$first_expect"

name "Parse interspersed brackets"
first=`echo "3 (6 5) 3 (4 (5 (6 (7)) 4) (2 4))" | ../dist/build/kropaya-bootstrap-raw-parser/kropaya-bootstrap-raw-parser`
first_expect='Right [JustExpression (Expression [] (Right (CVJuxt [CVAtomicValue (IntValue 3),CVJuxt [CVAtomicValue (IntValue 6),CVAtomicValue (IntValue 5)],CVAtomicValue (IntValue 3),CVJuxt [CVAtomicValue (IntValue 4),CVJuxt [CVAtomicValue (IntValue 5),CVJuxt [CVAtomicValue (IntValue 6),CVAtomicValue (IntValue 7)],CVAtomicValue (IntValue 4)],CVJuxt [CVAtomicValue (IntValue 2),CVAtomicValue (IntValue 4)]]])))]'
expect_eq "$first" "$first_expect"
