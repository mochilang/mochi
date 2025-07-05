import ast
import sys

class Conv(ast.NodeVisitor):
    def __init__(self):
        self.lines = []
        self.indent = 0

    def emit(self, line):
        self.lines.append("  " * self.indent + line)

    def convert(self, node):
        self.visit(node)
        if self.lines and self.lines[-1] != "":
            self.lines.append("")
        return "\n".join(self.lines)

    def visit_Module(self, node):
        for stmt in node.body:
            self.visit(stmt)

    def visit_FunctionDef(self, node):
        args = ",".join(a.arg for a in node.args.args)
        self.emit(f"fun {node.name}({args}): any {{")
        self.indent += 1
        for s in node.body:
            self.visit(s)
        self.indent -= 1
        self.emit("}")

    def visit_Return(self, node):
        self.emit("return " + self.expr(node.value))

    def visit_Assign(self, node):
        target = self.expr(node.targets[0])
        self.emit(f"let {target} = {self.expr(node.value)}")

    def visit_Expr(self, node):
        self.emit(self.expr(node.value))

    def visit_For(self, node):
        self.emit(f"for {self.expr(node.target)} in {self.expr(node.iter)} {{")
        self.indent += 1
        for s in node.body:
            self.visit(s)
        self.indent -= 1
        self.emit("}")

    def expr(self, node):
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Constant):
            return repr(node.value)
        if isinstance(node, ast.BinOp):
            return f"{self.expr(node.left)} {self.op(node.op)} {self.expr(node.right)}"
        if isinstance(node, ast.Call):
            args = ",".join(self.expr(a) for a in node.args)
            return f"{self.expr(node.func)}({args})"
        return "?"

    def op(self, op):
        if isinstance(op, ast.Add):
            return "+"
        if isinstance(op, ast.Sub):
            return "-"
        if isinstance(op, ast.Mult):
            return "*"
        if isinstance(op, ast.Div):
            return "/"
        if isinstance(op, ast.Mod):
            return "%"
        return "?"

src = open(sys.argv[1]).read()
print(Conv().convert(ast.parse(src)))
