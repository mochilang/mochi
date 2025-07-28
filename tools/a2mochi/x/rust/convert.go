//go:build slow

package rust

import (
	mochiast "mochi/ast"
	rs "mochi/tools/a2mochi/x/rs"
)

type ASTNode = rs.ASTNode

func ParseAST(src string) (*ASTNode, error)      { return rs.ParseAST(src) }
func Parse(src string) (*ASTNode, error)         { return rs.Parse(src) }
func ParseFile(path string) (*ASTNode, error)    { return rs.ParseFile(path) }
func ParseASTFile(path string) (*ASTNode, error) { return rs.ParseASTFile(path) }
func MarshalAST(ast *ASTNode) ([]byte, error)    { return rs.MarshalAST(ast) }

func ConvertAST(src string, n *ASTNode) (*mochiast.Node, error) { return rs.ConvertAST(src, n) }
func Convert(src string) (*mochiast.Node, error)                { return rs.Convert(src) }
func ConvertFile(path string) (*mochiast.Node, error)           { return rs.ConvertFile(path) }
func ConvertASTFile(path string) (*mochiast.Node, error)        { return rs.ConvertASTFile(path) }
func ConvertSourceAST(src string, n *ASTNode) (string, error)   { return rs.ConvertSourceAST(src, n) }
func ConvertSource(src string) (string, error)                  { return rs.ConvertSource(src) }
func ConvertFileSource(path string) (string, error)             { return rs.ConvertFileSource(path) }
func ConvertASTFileSource(path string) (string, error)          { return rs.ConvertASTFileSource(path) }
