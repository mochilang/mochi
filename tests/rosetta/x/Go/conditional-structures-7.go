//go:build ignore
// +build ignore

switch x := fetch(); {
case x == "cheese":
    statements
case otherBooleanExpression:
    other
    statements
}
