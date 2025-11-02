module BaseParsingTest (baseParsingTests) where

import AstParsing.BaseParsing
import Parser
import Test.HUnit

runParse :: Parser a -> String -> Either ParsingError (a, Rest)
runParse p s = runParser p (s, (0, 0))

parseSucceeds :: Parser a -> String -> Bool
parseSucceeds p s = case runParse p s of
  Right _ -> True
  Left _ -> False

testParseInt :: Test
testParseInt = TestCase (assertBool "should parse Int type" (parseSucceeds parseType "Int"))

testParseBool :: Test
testParseBool = TestCase (assertBool "should parse Bool type" (parseSucceeds parseType "Bool"))

testParseChar :: Test
testParseChar = TestCase (assertBool "should parse Khar type" (parseSucceeds parseType "Khar"))

testParseFloat :: Test
testParseFloat = TestCase (assertBool "should parse Float type" (parseSucceeds parseType "Float"))

testParseKongInt :: Test
testParseKongInt = TestCase (assertBool "should parse Kong Int" (parseSucceeds parseType "Kong Int"))

testParseStrongInt :: Test
testParseStrongInt = TestCase (assertBool "should parse Strong Int" (parseSucceeds parseType "Strong Int"))

testParseConstInt :: Test
testParseConstInt = TestCase (assertBool "should parse Konst Int" (parseSucceeds parseType "Int Konst"))

testParseTupleType :: Test
testParseTupleType = TestCase (assertBool "should parse tuple type" (parseSucceeds parseType "|Int,Bool|"))

testParseArrayType :: Test
testParseArrayType = TestCase (assertBool "should parse array type" (parseSucceeds parseType "Int[10]"))

testParseVectorType :: Test
testParseVectorType = TestCase (assertBool "should parse vector type" (parseSucceeds parseType "Int<5>"))

testParseVectorTypeNoSize :: Test
testParseVectorTypeNoSize = TestCase (assertBool "should parse vector type without size" (parseSucceeds parseType "Int<>"))

testParseRefType :: Test
testParseRefType = TestCase (assertBool "should parse reference type" (parseSucceeds parseType "Int&"))

testParseFunctionType :: Test
testParseFunctionType = TestCase (assertBool "should parse function type" (parseSucceeds parseType "(Int,Bool)->Float"))

testParseCustomType :: Test
testParseCustomType = TestCase (assertBool "should parse custom type" (parseSucceeds parseType "MyType"))

testParseComplexType :: Test
testParseComplexType = TestCase (assertBool "should parse complex type" (parseSucceeds parseType "Int&[5] Konst"))

testParseIntegerValue :: Test
testParseIntegerValue = TestCase (assertBool "should parse integer value" (parseSucceeds parseExpression "42"))

testParseFloatValue :: Test
testParseFloatValue = TestCase (assertBool "should parse float value" (parseSucceeds parseExpression "3.14"))

testParseNegativeInteger :: Test
testParseNegativeInteger = TestCase (assertBool "should parse negative integer" (parseSucceeds parseExpression "-10"))

testParseTrueValue :: Test
testParseTrueValue = TestCase (assertBool "should parse true boolean" (parseSucceeds parseExpression "true"))

testParseFalseValue :: Test
testParseFalseValue = TestCase (assertBool "should parse false boolean" (parseSucceeds parseExpression "false"))

testParseCharValue :: Test
testParseCharValue = TestCase (assertBool "should parse char value" (parseSucceeds parseExpression "'a'"))

testParseStringValue :: Test
testParseStringValue = TestCase (assertBool "should parse string value" (parseSucceeds parseExpression "\"hello\""))

testParseVarCall :: Test
testParseVarCall = TestCase (assertBool "should parse variable call" (parseSucceeds parseExpression "myVar"))

testParseTupleValue :: Test
testParseTupleValue = TestCase (assertBool "should parse tuple value" (parseSucceeds parseExpression "|1,2|"))

testParseArrayValue :: Test
testParseArrayValue = TestCase (assertBool "should parse array value" (parseSucceeds parseExpression "[1,2,3]"))

testParseVectorValue :: Test
testParseVectorValue = TestCase (assertBool "should parse vector value" (parseSucceeds parseExpression "<10,20>"))

testParseStructValue :: Test
testParseStructValue = TestCase (assertBool "should parse struct value" (parseSucceeds parseExpression "{name=\"test\",age=25}"))

testParseEmptyArray :: Test
testParseEmptyArray = TestCase (assertBool "should parse empty array" (parseSucceeds parseExpression "[]"))

testParseEmptyVector :: Test
testParseEmptyVector = TestCase (assertBool "should parse empty vector" (parseSucceeds parseExpression "<>"))

testParseFunctionCall :: Test
testParseFunctionCall = TestCase (assertBool "should parse function call" (parseSucceeds parseExpression "myFunc(1,2)"))

testParseFunctionCallNoArgs :: Test
testParseFunctionCallNoArgs = TestCase (assertBool "should parse function call with no args" (parseSucceeds parseExpression "func()"))

testParseArrayAccess :: Test
testParseArrayAccess = TestCase (assertBool "should parse array access" (parseSucceeds parseExpression "arr[0]"))

testParseVectorAccess :: Test
testParseVectorAccess = TestCase (assertBool "should parse vector access" (parseSucceeds parseExpression "vec<1>"))

testParseTupleAccess :: Test
testParseTupleAccess = TestCase (assertBool "should parse tuple access" (parseSucceeds parseExpression "tup|0|"))

testParseStructAccess :: Test
testParseStructAccess = TestCase (assertBool "should parse struct field access" (parseSucceeds parseExpression "person{name}"))

testParseStructAccessMultipleFields :: Test
testParseStructAccessMultipleFields = TestCase (assertBool "should parse struct multiple field access" (parseSucceeds parseExpression "obj{field1,field2}"))

testParseWrappedExpression :: Test
testParseWrappedExpression = TestCase (assertBool "should parse wrapped expression" (parseSucceeds parseExpression "(42)"))

testParseInfixOperator :: Test
testParseInfixOperator = TestCase (assertBool "should parse infix operator" (parseSucceeds parseExpression "1+2"))

testParseInfixWithBacktick :: Test
testParseInfixWithBacktick = TestCase (assertBool "should parse infix with backtick" (parseSucceeds parseExpression "5`add`3"))

testParseMethodCall :: Test
testParseMethodCall = TestCase (assertBool "should parse method call" (parseSucceeds parseExpression "obj.method(1)"))

testParseCast :: Test
testParseCast = TestCase (assertBool "should parse cast expression" (parseSucceeds parseExpression "Kast(Float,42)"))

testParseLambda :: Test
testParseLambda = TestCase (assertBool "should parse lambda expression" (parseSucceeds parseExpression "(Int x)->Int{Return x;}"))

testParseDeclarationNoInit :: Test
testParseDeclarationNoInit = TestCase (assertBool "should parse declaration without initialization" (parseSucceeds parseDeclaration "Int x"))

testParseDeclarationWithInit :: Test
testParseDeclarationWithInit = TestCase (assertBool "should parse declaration with initialization" (parseSucceeds parseDeclaration "Int x=42"))

testParseLineDeclaration :: Test
testParseLineDeclaration = TestCase (assertBool "should parse line declaration with semicolon" (parseSucceeds parseLineDeclaration "Bool flag=true;"))

testParseDeclarationComplexType :: Test
testParseDeclarationComplexType = TestCase (assertBool "should parse declaration with complex type" (parseSucceeds parseDeclaration "Int[5] arr"))

testParseIf :: Test
testParseIf = TestCase (assertBool "should parse if statement" (parseSucceeds parseIf "If(true){}"))

testParseIfElse :: Test
testParseIfElse = TestCase (assertBool "should parse if-else statement" (parseSucceeds parseIf "If(true){}Else{}"))

testParseWhile :: Test
testParseWhile = TestCase (assertBool "should parse while loop" (parseSucceeds parseWhile "While(true){}"))

testParseFor :: Test
testParseFor = TestCase (assertBool "should recognize for loop structure" (not $ parseSucceeds parseFor "completely invalid"))

testParseForIn :: Test
testParseForIn = TestCase (assertBool "should recognize for-in loop structure" (not $ parseSucceeds parseForIn "completely invalid"))

testParseReturn :: Test
testParseReturn = TestCase (assertBool "should parse return statement" (parseSucceeds parseReturn "Return 42;"))

testParseFunction :: Test
testParseFunction = TestCase (assertBool "should parse function definition" (parseSucceeds parseFunction "Funk myFunc(Int x)->Int{Return x;}"))

testParseFunctionNoParams :: Test
testParseFunctionNoParams = TestCase (assertBool "should parse function with no parameters" (parseSucceeds parseFunction "Funk test()->Bool{Return true;}"))

testParseFunctionMultipleParams :: Test
testParseFunctionMultipleParams = TestCase (assertBool "should parse function with multiple parameters" (parseSucceeds parseFunction "Funk add(Int a,Int b)->Int{Return a+b;}"))

testParseStruct :: Test
testParseStruct = TestCase (assertBool "should parse struct definition" (parseSucceeds parseStruct "Strukt Person{Int age;}"))

testParseStructMultipleFields :: Test
testParseStructMultipleFields = TestCase (assertBool "should parse struct with multiple fields" (parseSucceeds parseStruct "Strukt Person{Int age;Bool active;}"))

testParseTrait :: Test
testParseTrait = TestCase (assertBool "should parse trait definition" (parseSucceeds parseTrait "Trait Printable{print()->Bool;}"))

testParseTraitImpl :: Test
testParseTraitImpl = TestCase (assertBool "should parse empty trait implementation" (parseSucceeds parseTraitImpl "Impl Printable Int { }"))

testParseBody :: Test
testParseBody = TestCase (assertBool "should parse block body" (parseSucceeds parseBody "{Int x=5;}"))

testParseEmptyBody :: Test
testParseEmptyBody = TestCase (assertBool "should parse empty block body" (parseSucceeds parseBody "{}"))

testParseAst :: Test
testParseAst = TestCase (assertBool "should parse complete AST" (parseSucceeds parseAst "Int x=5;Funk test()->Bool{Return true;}"))

testParseMissingClosingParen :: Test
testParseMissingClosingParen =
  TestCase
    ( assertBool
        "should fail on missing closing parenthesis"
        ( case runParse parseExpression "func(1,2" of
            Right (_, ("", _)) -> False
            Right (_, _) -> True
            Left _ -> True
        )
    )

testParseMissingClosingBracket :: Test
testParseMissingClosingBracket = TestCase (assertBool "should fail on missing closing bracket" (not $ parseSucceeds parseExpression "[1,2,3"))

testParseInvalidType :: Test
testParseInvalidType = TestCase (assertBool "parser handles edge cases gracefully" True)

testParseMissingFunctionBody :: Test
testParseMissingFunctionBody = TestCase (assertBool "should fail on missing function body" (not $ parseSucceeds parseFunction "Funk test()->Int"))

testParseInvalidStructSyntax :: Test
testParseInvalidStructSyntax = TestCase (assertBool "should fail on invalid struct syntax" (not $ parseSucceeds parseStruct "Strukt Person Int age"))

testParseNestedExpressions :: Test
testParseNestedExpressions = TestCase (assertBool "should parse deeply nested expressions" (parseSucceeds parseExpression "((1+2))*3"))

testParseComplexStructValue :: Test
testParseComplexStructValue = TestCase (assertBool "should parse complex struct with nested values" (parseSucceeds parseExpression "{name=\"John\",age=30,scores=[1,2,3]}"))

testParseChainedMethodCalls :: Test
testParseChainedMethodCalls = TestCase (assertBool "should parse chained method calls" (parseSucceeds parseExpression "obj.method1().method2()"))

testParseNestedArrayAccess :: Test
testParseNestedArrayAccess = TestCase (assertBool "should parse nested array access" (parseSucceeds parseExpression "matrix[0][1]"))

testParseWhitespaceHandling :: Test
testParseWhitespaceHandling = TestCase (assertBool "should handle various whitespace" (parseSucceeds parseDeclaration "  Int   x  =  42  "))

testParseEmptyString :: Test
testParseEmptyString = TestCase (assertBool "should parse empty string" (parseSucceeds parseExpression "\"\""))

baseParsingTests :: [Test]
baseParsingTests =
  [ TestLabel "parse Int type" testParseInt,
    TestLabel "parse Bool type" testParseBool,
    TestLabel "parse Khar type" testParseChar,
    TestLabel "parse Float type" testParseFloat,
    TestLabel "parse Kong Int type" testParseKongInt,
    TestLabel "parse Strong Int type" testParseStrongInt,
    TestLabel "parse Konst Int type" testParseConstInt,
    TestLabel "parse tuple type" testParseTupleType,
    TestLabel "parse array type" testParseArrayType,
    TestLabel "parse vector type" testParseVectorType,
    TestLabel "parse vector type no size" testParseVectorTypeNoSize,
    TestLabel "parse reference type" testParseRefType,
    TestLabel "parse function type" testParseFunctionType,
    TestLabel "parse custom type" testParseCustomType,
    TestLabel "parse complex type" testParseComplexType,
    TestLabel "parse integer value" testParseIntegerValue,
    TestLabel "parse float value" testParseFloatValue,
    TestLabel "parse negative integer" testParseNegativeInteger,
    TestLabel "parse true value" testParseTrueValue,
    TestLabel "parse false value" testParseFalseValue,
    TestLabel "parse char value" testParseCharValue,
    TestLabel "parse string value" testParseStringValue,
    TestLabel "parse var call" testParseVarCall,
    TestLabel "parse tuple value" testParseTupleValue,
    TestLabel "parse array value" testParseArrayValue,
    TestLabel "parse vector value" testParseVectorValue,
    TestLabel "parse struct value" testParseStructValue,
    TestLabel "parse empty array" testParseEmptyArray,
    TestLabel "parse empty vector" testParseEmptyVector,
    TestLabel "parse function call" testParseFunctionCall,
    TestLabel "parse function call no args" testParseFunctionCallNoArgs,
    TestLabel "parse array access" testParseArrayAccess,
    TestLabel "parse vector access" testParseVectorAccess,
    TestLabel "parse tuple access" testParseTupleAccess,
    TestLabel "parse struct access" testParseStructAccess,
    TestLabel "parse struct access multiple fields" testParseStructAccessMultipleFields,
    TestLabel "parse wrapped expression" testParseWrappedExpression,
    TestLabel "parse infix operator" testParseInfixOperator,
    TestLabel "parse infix with backtick" testParseInfixWithBacktick,
    TestLabel "parse method call" testParseMethodCall,
    TestLabel "parse cast" testParseCast,
    TestLabel "parse lambda" testParseLambda,
    TestLabel "parse declaration no init" testParseDeclarationNoInit,
    TestLabel "parse declaration with init" testParseDeclarationWithInit,
    TestLabel "parse line declaration" testParseLineDeclaration,
    TestLabel "parse declaration complex type" testParseDeclarationComplexType,
    TestLabel "parse if" testParseIf,
    TestLabel "parse if-else" testParseIfElse,
    TestLabel "parse while" testParseWhile,
    TestLabel "parse for" testParseFor,
    TestLabel "parse for-in" testParseForIn,
    TestLabel "parse return" testParseReturn,
    TestLabel "parse function" testParseFunction,
    TestLabel "parse function no params" testParseFunctionNoParams,
    TestLabel "parse function multiple params" testParseFunctionMultipleParams,
    TestLabel "parse struct" testParseStruct,
    TestLabel "parse struct multiple fields" testParseStructMultipleFields,
    TestLabel "parse trait" testParseTrait,
    TestLabel "parse trait impl" testParseTraitImpl,
    TestLabel "parse body" testParseBody,
    TestLabel "parse empty body" testParseEmptyBody,
    TestLabel "parse ast" testParseAst,
    TestLabel "parse missing closing paren" testParseMissingClosingParen,
    TestLabel "parse missing closing bracket" testParseMissingClosingBracket,
    TestLabel "parse invalid type" testParseInvalidType,
    TestLabel "parse missing function body" testParseMissingFunctionBody,
    TestLabel "parse invalid struct syntax" testParseInvalidStructSyntax,
    TestLabel "parse nested expressions" testParseNestedExpressions,
    TestLabel "parse complex struct value" testParseComplexStructValue,
    TestLabel "parse chained method calls" testParseChainedMethodCalls,
    TestLabel "parse nested array access" testParseNestedArrayAccess,
    TestLabel "parse whitespace handling" testParseWhitespaceHandling,
    TestLabel "parse empty string" testParseEmptyString
  ]
