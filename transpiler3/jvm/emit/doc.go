// Package emit serialises javasrc AST nodes to Java source text and
// invokes javac to produce .class files. Entry point: Emit(cu *javasrc.CompilationUnit, workDir string).
// Adjacent packages: transpiler3/jvm/javasrc (input), transpiler3/jvm/build (caller).
package emit
