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

    def visit_While(self, node):
        self.emit(f"while {self.expr(node.test)} {{")
        self.indent += 1
        for s in node.body:
            self.visit(s)
        self.indent -= 1
        self.emit("}")

    def visit_If(self, node):
        self.emit(f"if {self.expr(node.test)} {{")
        self.indent += 1
        for s in node.body:
            self.visit(s)
        self.indent -= 1
        if node.orelse:
            self.emit("} else {")
            self.indent += 1
            for s in node.orelse:
                self.visit(s)
            self.indent -= 1
        self.emit("}")

    def visit_Break(self, node):
        self.emit("break")

    def visit_Continue(self, node):
        self.emit("continue")

    def expr(self, node):
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Constant):
            return repr(node.value)
        if isinstance(node, ast.UnaryOp):
            return f"{self.uop(node.op)}{self.expr(node.operand)}"
        if isinstance(node, ast.BoolOp):
            op = ' and ' if isinstance(node.op, ast.And) else ' or '
            return op.join(self.expr(v) for v in node.values)
        if isinstance(node, ast.BinOp):
            return f"{self.expr(node.left)} {self.op(node.op)} {self.expr(node.right)}"
        if isinstance(node, ast.Compare):
            left = self.expr(node.left)
            parts = []
            for o, c in zip(node.ops, node.comparators):
                parts.append(f"{self.cmp(o)} {self.expr(c)}")
            return f"{left} {' '.join(parts)}"
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

    def uop(self, op):
        if isinstance(op, ast.USub):
            return "-"
        if isinstance(op, ast.Not):
            return "not "
        return ""

    def cmp(self, op):
        if isinstance(op, ast.Eq):
            return "=="
        if isinstance(op, ast.NotEq):
            return "!="
        if isinstance(op, ast.Lt):
            return "<"
        if isinstance(op, ast.LtE):
            return "<="
        if isinstance(op, ast.Gt):
            return ">"
        if isinstance(op, ast.GtE):
            return ">="
        return "?"

src = open(sys.argv[1]).read()
print(Conv().convert(ast.parse(src)))
