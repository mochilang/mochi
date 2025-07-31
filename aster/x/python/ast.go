package python

import py "mochi/aster/x/py"

// Program represents a parsed Python source file.
type Program = py.Program

// Node and typed aliases mirror the structures from the py package.
type (
	Node                = py.Node
	Module              = py.Module
	Comment             = py.Comment
	DecoratedDefinition = py.DecoratedDefinition
	Decorator           = py.Decorator
	ClassDefinition     = py.ClassDefinition
	FunctionDefinition  = py.FunctionDefinition
	Parameters          = py.Parameters
	ExpressionStatement = py.ExpressionStatement
	Assignment          = py.Assignment
	ReturnStatement     = py.ReturnStatement
	Identifier          = py.Identifier
	Integer             = py.Integer
	Float               = py.Float
	String              = py.String
	True                = py.True
	False               = py.False
	None                = py.None
	Call                = py.Call
	Attribute           = py.Attribute
	List                = py.List
	ListComprehension   = py.ListComprehension
	Dictionary          = py.Dictionary
	Pair                = py.Pair
	ForStatement        = py.ForStatement
	WhileStatement      = py.WhileStatement
	ForInClause         = py.ForInClause
	ArgumentList        = py.ArgumentList
	Lambda              = py.Lambda
	LambdaParameters    = py.LambdaParameters
	Type                = py.Type
)

// Option controls AST generation. It mirrors py.Option.
type Option = py.Option
