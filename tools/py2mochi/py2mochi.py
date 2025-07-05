#!/usr/bin/env python3
import argparse
import ast
import json
import os


class Converter(ast.NodeVisitor):
    def __init__(self):
        self.lines: list[str] = []
        self.indent = 0
        self.dataclasses: set[str] = set()
        self.seen_assigns: set[str] = set()
        self.current_callable: tuple[list[str], str] | None = None
        self.structs: dict[str, list[tuple[str, str]]] = {}
        self.unions: dict[str, list[tuple[str, list[tuple[str, str]]]]] = {}

    def emit(self, line: str) -> None:
        self.lines.append("  " * self.indent + line)

    # ----- helpers for expressions -----

    def convert_type(self, node: ast.expr | None) -> str:
        if node is None:
            return "any"
        if isinstance(node, ast.Name):
            mapping = {"int": "int", "str": "string", "bool": "bool", "float": "float"}
            return mapping.get(node.id, node.id)
        if isinstance(node, ast.Attribute):
            return node.attr
        if isinstance(node, ast.Subscript):
            if isinstance(node.value, ast.Attribute) and node.value.attr == "Callable":
                if isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 2:
                    args, ret = node.slice.elts
                    if isinstance(args, ast.List):
                        arg_types = [self.convert_type(e) for e in args.elts]
                    else:
                        arg_types = [self.convert_type(args)]
                    return (
                        "fun(" + ", ".join(arg_types) + "): " + self.convert_type(ret)
                    )
        return "any"

    def parse_callable(self, node: ast.expr | None) -> tuple[list[str], str] | None:
        if not isinstance(node, ast.Subscript):
            return None
        if not (
            isinstance(node.value, ast.Attribute) and node.value.attr == "Callable"
        ):
            return None
        if not (isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 2):
            return None
        args_node, ret_node = node.slice.elts
        if isinstance(args_node, ast.List):
            arg_types = [self.convert_type(e) for e in args_node.elts]
        else:
            arg_types = [self.convert_type(args_node)]
        ret_type = self.convert_type(ret_node)
        return arg_types, ret_type

    def convert_expr(self, node: ast.expr) -> str:
        if isinstance(node, ast.Constant):
            if isinstance(node.value, str):
                return json.dumps(node.value)
            return str(node.value)
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Attribute):
            return f"{self.convert_expr(node.value)}.{node.attr}"
        if isinstance(node, ast.Subscript):
            target = self.convert_expr(node.value)
            sl = node.slice
            if isinstance(sl, ast.Slice):
                start = self.convert_expr(sl.lower) if sl.lower else ""
                stop = self.convert_expr(sl.upper) if sl.upper else ""
                return f"{target}[{start}:{stop}]"
            idx = getattr(sl, "value", sl)
            return f"{target}[{self.convert_expr(idx)}]"
        if isinstance(node, ast.BinOp):
            op_map = {ast.Add: "+", ast.Sub: "-", ast.Mult: "*", ast.Div: "/"}
            op = op_map.get(type(node.op), "?")
            return (
                f"{self.convert_expr(node.left)} {op} {self.convert_expr(node.right)}"
            )
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
            return "-" + self.convert_expr(node.operand)
        if isinstance(node, ast.BoolOp):
            if isinstance(node.op, ast.And):
                op = " and "
            else:
                op = " or "
            return op.join(self.convert_expr(v) for v in node.values)
        if isinstance(node, ast.Compare):
            op_map = {
                ast.Gt: ">",
                ast.Lt: "<",
                ast.GtE: ">=",
                ast.LtE: "<=",
                ast.Eq: "==",
                ast.NotEq: "!=",
            }
            left = self.convert_expr(node.left)
            op = op_map.get(type(node.ops[0]), "?")
            right = self.convert_expr(node.comparators[0])
            return f"{left} {op} {right}"
        if isinstance(node, ast.Call):
            func = self.convert_expr(node.func)
            if func in self.dataclasses:
                fields = [
                    f"{k.arg}: {self.convert_expr(k.value)}" for k in node.keywords
                ]
                return f"{func} {{ " + ", ".join(fields) + " }"
            args = [self.convert_expr(a) for a in node.args]
            args += [f"{k.arg}: {self.convert_expr(k.value)}" for k in node.keywords if k.arg]
            args += [self.convert_expr(k.value) for k in node.keywords if k.arg is None]
            return f"{func}(" + ", ".join(args) + ")"
        if isinstance(node, ast.Dict):
            items = [
                f"{self.convert_expr(k)}: {self.convert_expr(v)}"
                for k, v in zip(node.keys, node.values)
            ]
            return "{" + ", ".join(items) + "}"
        if isinstance(node, ast.Tuple):
            return "(" + ", ".join(self.convert_expr(e) for e in node.elts) + ")"
        if isinstance(node, ast.Starred):
            return self.convert_expr(node.value)
        if isinstance(node, ast.List):
            return "[" + ", ".join(self.convert_expr(e) for e in node.elts) + "]"
        if isinstance(node, ast.ListComp):
            return self.convert_list_comp(node)
        if isinstance(node, ast.Lambda):
            return self.convert_lambda(node)
        return "<expr>"

    def convert_lambda(
        self,
        node: ast.Lambda,
        annotated_args: list[str] | None = None,
        ret_type: str | None = None,
    ) -> str:
        parts = []
        for i, a in enumerate(node.args.args):
            typ = (
                annotated_args[i]
                if annotated_args and i < len(annotated_args)
                else None
            )
            if typ:
                parts.append(f"{a.arg}: {typ}")
            else:
                parts.append(a.arg)
        body = self.convert_expr(node.body)
        if ret_type:
            return f"fun({', '.join(parts)}): {ret_type} => {body}"
        return f"fun({', '.join(parts)}) => {body}"

    def convert_list_comp(self, node: ast.ListComp) -> str:
        parts: list[str] = []
        for gen in node.generators:
            parts.append(
                f"from {self.convert_expr(gen.target)} in {self.convert_expr(gen.iter)}"
            )
        ifs: list[str] = []
        for gen in node.generators:
            for if_ in gen.ifs:
                ifs.append(self.convert_expr(if_))
        if ifs:
            parts.append("where " + " and ".join(ifs))
        parts.append("select " + self.convert_expr(node.elt))
        result = parts[0]
        indent = "            "
        for part in parts[1:]:
            result += "\n" + indent + part
        return result

    # ----- visitors for statements -----

    def visit_Module(self, node: ast.Module) -> None:
        # first collect dataclass information
        for stmt in node.body:
            if isinstance(stmt, ast.ClassDef):
                dec_names = [
                    getattr(d, "id", None) or getattr(d, "attr", None)
                    for d in stmt.decorator_list
                ]
                if "dataclass" not in dec_names:
                    continue
                fields: list[tuple[str, str]] = []
                for sub in stmt.body:
                    if isinstance(sub, ast.AnnAssign) and isinstance(
                        sub.target, ast.Name
                    ):
                        fields.append(
                            (sub.target.id, self.convert_type(sub.annotation))
                        )
                base = stmt.bases[0].id if stmt.bases else None
                self.dataclasses.add(stmt.name)
                if base:
                    self.unions.setdefault(base, []).append((stmt.name, fields))
                else:
                    self.structs[stmt.name] = fields

        # emit structs
        for name, fields in self.structs.items():
            self.emit(f"type {name} {{")
            self.indent += 1
            for n, t in fields:
                self.emit(f"{n}: {t}")
            self.indent -= 1
            self.emit("}")

        # emit unions
        for base, variants in self.unions.items():
            self.emit(f"type {base} =")
            self.indent += 1
            for i, (name, fields) in enumerate(variants):
                field_str = ", ".join(f"{n}: {t}" for n, t in fields)
                if field_str:
                    self.emit(
                        f"{name}({field_str})" + (" |" if i < len(variants) - 1 else "")
                    )
                else:
                    self.emit(f"{name} {{}}" + (" |" if i < len(variants) - 1 else ""))
            self.indent -= 1

        # now handle remaining statements
        for stmt in node.body:
            if (
                isinstance(stmt, ast.If)
                and isinstance(stmt.test, ast.Compare)
                and isinstance(stmt.test.left, ast.Name)
                and stmt.test.left.id == "__name__"
            ):
                continue
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "main":
                for sub in stmt.body:
                    if isinstance(sub, ast.Global):
                        continue
                    self.visit(sub)
                continue
            if isinstance(stmt, ast.FunctionDef):
                self.visit(stmt)
                continue

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        dec_names = [
            getattr(d, "id", None) or getattr(d, "attr", None)
            for d in node.decorator_list
        ]
        if "dataclass" not in dec_names:
            return
        self.emit(f"type {node.name} {{")
        self.indent += 1
        for stmt in node.body:
            if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
                self.emit(f"{stmt.target.id}: {self.convert_type(stmt.annotation)}")
        self.indent -= 1
        self.emit("}")
        self.dataclasses.add(node.name)

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        if node.name == "main":
            for stmt in node.body:
                if isinstance(stmt, ast.Global):
                    continue
                self.visit(stmt)
            return
        args = [
            f"{arg.arg}: {self.convert_type(arg.annotation)}" for arg in node.args.args
        ]
        ret = self.convert_type(node.returns)
        self.emit(f"fun {node.name}({', '.join(args)}): {ret} {{")
        call_info = self.parse_callable(node.returns)
        prev = self.current_callable
        if call_info:
            self.current_callable = call_info
        self.indent += 1
        for stmt in node.body:
            self.visit(stmt)
        self.indent -= 1
        self.emit("}")
        self.current_callable = prev

    def visit_Return(self, node: ast.Return) -> None:
        if node.value is not None:
            if isinstance(node.value, ast.Lambda) and self.current_callable:
                args, ret = self.current_callable
                self.emit("return " + self.convert_lambda(node.value, args, ret))
            else:
                self.emit("return " + self.convert_expr(node.value))
        else:
            self.emit("return")

    def visit_Assign(self, node: ast.Assign) -> None:
        if len(node.targets) != 1:
            return
        target = node.targets[0]
        if isinstance(target, ast.Name):
            if isinstance(node.value, ast.Constant) and node.value.value is None:
                return
            if target.id in self.seen_assigns:
                return
            self.seen_assigns.add(target.id)
            self.emit(f"let {target.id} = {self.convert_expr(node.value)}")

    def visit_Expr(self, node: ast.Expr) -> None:
        if (
            isinstance(node.value, ast.Call)
            and isinstance(node.value.func, ast.Name)
            and node.value.func.id == "print"
        ):
            args = ", ".join(self.convert_expr(a) for a in node.value.args)
            self.emit(f"print({args})")
        else:
            self.emit(self.convert_expr(node.value))

    def visit_For(self, node: ast.For) -> None:
        target = self.convert_expr(node.target)
        iter_ = self.convert_expr(node.iter)
        self.emit(f"for {target} in {iter_} {{")
        self.indent += 1
        for stmt in node.body:
            self.visit(stmt)
        self.indent -= 1
        self.emit("}")

    def visit_If(self, node: ast.If) -> None:
        test = self.convert_expr(node.test)
        self.emit(f"if {test} {{")
        self.indent += 1
        for stmt in node.body:
            self.visit(stmt)
        self.indent -= 1
        if node.orelse:
            self.emit("} else {")
            self.indent += 1
            for stmt in node.orelse:
                self.visit(stmt)
            self.indent -= 1
            self.emit("}")
        else:
            self.emit("}")


def convert(path: str) -> str:
    with open(path, "r", encoding="utf-8") as f:
        src = f.read()
    tree = ast.parse(src)
    conv = Converter()
    conv.visit(tree)
    return "\n".join(conv.lines) + ("\n" if conv.lines else "")


def main():
    ap = argparse.ArgumentParser(description="Convert a subset of Python to Mochi")
    ap.add_argument("file")
    ap.add_argument("-o", "--out")
    args = ap.parse_args()
    code = convert(args.file)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as f:
            f.write(code)
    else:
        print(code, end="")


if __name__ == "__main__":
    main()
