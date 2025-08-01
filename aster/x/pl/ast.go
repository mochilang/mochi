package pl

import prolog "mochi/aster/x/prolog"

// Program represents a Prolog program AST.
// It is aliased to the more featureful representation used in the prolog package.
type Program = prolog.Program

// Node aliases the AST node type used by the prolog package.
type Node = prolog.Node

// Option controls how Inspect populates position information.
type Option = prolog.Option
