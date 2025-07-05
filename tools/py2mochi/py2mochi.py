#!/usr/bin/env python3
import argparse
import ast
import json
import os
import sys


class ConversionError(Exception):
    def __init__(self, msg: str, lineno: int, line: str):
        super().__init__(msg)
        self.lineno = lineno
        self.line = line


class Converter(ast.NodeVisitor):
    def __init__(self, src: str):
        self.lines: list[str] = []
        self.indent = 0
        self.src_lines = src.splitlines()
        self.dataclasses: set[str] = set()
        self.seen_assigns: set[str] = set()
        self.assign_values: dict[str, str] = {}
        self.current_callable: tuple[list[str], str] | None = None
        self.structs: dict[str, tuple[list[tuple[str, str]], list[ast.FunctionDef]]] = {}
        self.unions: dict[str, list[tuple[str, list[tuple[str, str]]]]] = {}
        self.name_map = {"_next": "next"}

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
            return self.name_map.get(node.id, node.id)
        if isinstance(node, ast.Attribute):
            if (
                node.attr == "lower"
                and isinstance(node.value, ast.Call)
                and isinstance(node.value.func, ast.Name)
                and node.value.func.id == "str"
                and len(node.value.args) == 1
            ):
                return self.convert_expr(node.value.args[0])
            if isinstance(node.value, ast.Name) and node.value.id == "self":
                return node.attr
            return f"{self.convert_expr(node.value)}.{node.attr}"
        if isinstance(node, ast.Subscript):
            target = self.convert_expr(node.value)
            sl = node.slice
            if isinstance(sl, ast.Slice):
                start = self.convert_expr(sl.lower) if sl.lower else ""
                stop = self.convert_expr(sl.upper) if sl.upper else ""
                return f"{target}[{start}:{stop}]"
            return f"{target}[{self.convert_expr(sl)}]"
        if isinstance(node, ast.BinOp):
            op_map = {ast.Add: "+", ast.Sub: "-", ast.Mult: "*", ast.Div: "/"}
            op = op_map.get(type(node.op), "?")
            return (
                f"{self.convert_expr(node.left)} {op} {self.convert_expr(node.right)}"
            )
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
            return "-" + self.convert_expr(node.operand)
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.Not):
            return "!" + self.convert_expr(node.operand)
        if isinstance(node, ast.BoolOp):
            if isinstance(node.op, ast.And):
                op = " and "
            else:
                op = " or "
            return op.join(self.convert_expr(v) for v in node.values)
        if isinstance(node, ast.IfExp):
            test = self.convert_expr(node.test)
            body = self.convert_expr(node.body)
            orelse = self.convert_expr(node.orelse)
            return f"if {test} then {body} else {orelse}"
        if isinstance(node, ast.Compare):
            op_map = {
                ast.Gt: ">",
                ast.Lt: "<",
                ast.GtE: ">=",
                ast.LtE: "<=",
                ast.Eq: "==",
                ast.NotEq: "!=",
                ast.In: "?",
                ast.NotIn: "!?",
            }
            left = self.convert_expr(node.left)
            op = op_map.get(type(node.ops[0]), "?")
            right = self.convert_expr(node.comparators[0])
            return f"{left} {op} {right}"
        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name):
                if (
                    node.func.id == "_get"
                    and len(node.args) == 2
                    and isinstance(node.args[1], ast.Constant)
                    and isinstance(node.args[1].value, str)
                ):
                    obj = self.convert_expr(node.args[0])
                    return f"{obj}.{node.args[1].value}"
                if node.func.id == "_fetch" and len(node.args) >= 1:
                    url = self.convert_expr(node.args[0])
                    if len(node.args) > 1 and not (
                        isinstance(node.args[1], ast.Constant) and node.args[1].value is None
                    ):
                        opts = self.convert_expr(node.args[1])
                        return f"fetch {url} with {opts}"
                    return f"fetch {url}"
                if node.func.id == "_load" and len(node.args) >= 1:
                    path = self.convert_expr(node.args[0])
                    base = "load" if path == "None" else f"load {path}"
                    if len(node.args) > 1 and not (
                        isinstance(node.args[1], ast.Constant) and node.args[1].value is None
                    ):
                        opts = self.convert_expr(node.args[1])
                        return f"{base} with {opts}"
                    return base
                if node.func.id == "_save" and len(node.args) >= 1:
                    target = self.convert_expr(node.args[0])
                    base = f"save {target}"
                    opts_arg = None
                    if len(node.args) > 2:
                        opts_arg = node.args[2]
                    elif len(node.args) > 1 and not (
                        isinstance(node.args[1], ast.Constant) and node.args[1].value is None
                    ):
                        opts_arg = node.args[1]
                    if opts_arg is not None and not (
                        isinstance(opts_arg, ast.Constant) and opts_arg.value is None
                    ):
                        opts = self.convert_expr(opts_arg)
                        return f"{base} with {opts}"
                    return base
            func = self.convert_expr(node.func)
            if func in self.dataclasses:
                if (
                    not node.args
                    and len(node.keywords) == 1
                    and node.keywords[0].arg is None
                ):
                    kw = node.keywords[0].value
                    if (
                        isinstance(kw, ast.Call)
                        and isinstance(kw.func, ast.Name)
                        and kw.func.id == "_fetch"
                    ):
                        url = self.convert_expr(kw.args[0])
                        if len(kw.args) > 1 and not (
                            isinstance(kw.args[1], ast.Constant) and kw.args[1].value is None
                        ):
                            opts = self.convert_expr(kw.args[1])
                            return f"fetch {url} with {opts} as {func}"
                        return f"fetch {url} as {func}"
                    if isinstance(kw, ast.Name):
                        return f"{func} {{ {kw.id} }}"
                fields = [
                    f"{k.arg}: {self.convert_expr(k.value)}" for k in node.keywords if k.arg
                ]
                return f"{func} {{ " + ", ".join(fields) + " }"
            args = [self.convert_expr(a) for a in node.args]
            args += [
                f"{k.arg}: {self.convert_expr(k.value)}" for k in node.keywords if k.arg
            ]
            if func == "dict" and len(node.args) == 1 and not node.keywords and isinstance(node.args[0], ast.Dict):
                return self.convert_expr(node.args[0])
            args += [self.convert_expr(k.value) for k in node.keywords if k.arg is None]
            return f"{func}(" + ", ".join(args) + ")"
        if isinstance(node, ast.Dict):
            items = []
            for k, v in zip(node.keys, node.values):
                key = self.convert_expr(k)
                if isinstance(k, ast.Constant) and isinstance(k.value, str):
                    key = k.value
                items.append(f"{key}: {self.convert_expr(v)}")
            return "{" + ", ".join(items) + "}"
        if isinstance(node, ast.DictComp):
            parts = [f"{self.convert_expr(node.key)}: {self.convert_expr(node.value)}"]
            for gen in node.generators:
                target = self.convert_expr(gen.target)
                iter_ = self.convert_expr(gen.iter)
                parts.append(f"for {target} in {iter_}")
                for if_ in gen.ifs:
                    parts.append(f"if {self.convert_expr(if_)}")
            return "{" + " ".join(parts) + "}"
        if isinstance(node, ast.Tuple):
            return "(" + ", ".join(self.convert_expr(e) for e in node.elts) + ")"
        if isinstance(node, ast.Starred):
            return self.convert_expr(node.value)
        if isinstance(node, ast.List):
            return "[" + ", ".join(self.convert_expr(e) for e in node.elts) + "]"
        if isinstance(node, ast.ListComp):
            return self.convert_list_comp(node)
        if isinstance(node, ast.GeneratorExp):
            fake = ast.ListComp(node.elt, node.generators)
            return self.convert_list_comp(fake)
        if isinstance(node, ast.Lambda):
            return self.convert_lambda(node)
        line = self.src_lines[getattr(node, "lineno", 1) - 1]
        raise ConversionError("unhandled expression", getattr(node, "lineno", 0), line)

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

        def parse_dataset_iter(it: ast.expr) -> tuple[str, str | None, str | None, str | None]:
            sort = None
            skip = None
            take = None
            cur = it

            # handle take
            if isinstance(cur, ast.Subscript) and isinstance(cur.slice, ast.Slice):
                sl = cur.slice
                if (
                    sl.lower is None
                    and sl.step is None
                    and isinstance(sl.upper, ast.Call)
                    and getattr(sl.upper.func, "id", None) == "max"
                    and len(sl.upper.args) == 2
                    and isinstance(sl.upper.args[1], ast.Constant)
                    and sl.upper.args[1].value == 0
                ):
                    take = self.convert_expr(sl.upper.args[0])
                    cur = cur.value

            # handle skip
            if isinstance(cur, ast.Subscript) and isinstance(cur.slice, ast.Slice):
                sl = cur.slice
                if (
                    sl.upper is None
                    and sl.step is None
                    and isinstance(sl.lower, ast.Call)
                    and getattr(sl.lower.func, "id", None) == "max"
                    and len(sl.lower.args) == 2
                    and isinstance(sl.lower.args[1], ast.Constant)
                    and sl.lower.args[1].value == 0
                ):
                    skip = self.convert_expr(sl.lower.args[0])
                    cur = cur.value

            # handle sort
            if isinstance(cur, ast.Call) and isinstance(cur.func, ast.Name) and cur.func.id == "sorted":
                if cur.keywords:
                    for kw in cur.keywords:
                        if kw.arg == "key" and isinstance(kw.value, ast.Lambda):
                            body = kw.value.body
                            if (
                                isinstance(body, ast.Call)
                                and isinstance(body.func, ast.Name)
                                and body.func.id == "_sort_key"
                                and body.args
                            ):
                                sort = self.convert_expr(body.args[0])
                            else:
                                sort = self.convert_expr(body)
                if cur.args:
                    cur = cur.args[0]

            # remove trivial list comp wrappers
            if (
                isinstance(cur, ast.ListComp)
                and len(cur.generators) == 1
                and isinstance(cur.generators[0].target, ast.Name)
                and isinstance(cur.elt, ast.Name)
                and cur.elt.id == cur.generators[0].target.id
                and not cur.generators[0].ifs
            ):
                cur = cur.generators[0].iter

            return self.convert_expr(cur), sort, skip, take

        if not node.generators:
            return "[]"

        first = node.generators[0]
        # special case: [T(**it) for it in _load(...)] -> load ... as T
        if (
            len(node.generators) == 1
            and isinstance(node.elt, ast.Call)
            and isinstance(node.elt.func, ast.Name)
            and node.elt.func.id in self.dataclasses
            and not node.elt.args
            and len(node.elt.keywords) == 1
            and node.elt.keywords[0].arg is None
            and isinstance(node.elt.keywords[0].value, ast.Name)
            and isinstance(first.target, ast.Name)
            and node.elt.keywords[0].value.id == first.target.id
            and isinstance(first.iter, ast.Call)
            and isinstance(first.iter.func, ast.Name)
            and first.iter.func.id == "_load"
        ):
            load_expr = self.convert_expr(first.iter)
            typ = node.elt.func.id
            if load_expr.startswith("load"):
                if " with " in load_expr:
                    base, rest = load_expr.split(" with ", 1)
                    return f"{base} as {typ} with {rest}"
                return f"{load_expr} as {typ}"

        base_iter, sort, skip, take = parse_dataset_iter(first.iter)
        parts.append(f"from {self.convert_expr(first.target)} in {base_iter}")

        for gen in node.generators[1:]:
            parts.append(
                f"from {self.convert_expr(gen.target)} in {self.convert_expr(gen.iter)}"
            )

        ifs: list[str] = []
        for gen in node.generators:
            for if_ in gen.ifs:
                ifs.append(self.convert_expr(if_))
        if ifs:
            parts.append("where " + " and ".join(ifs))
        if sort:
            parts.append("sort by " + sort)
        if skip:
            parts.append("skip " + skip)
        if take:
            parts.append("take " + take)
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
                methods: list[ast.FunctionDef] = []
                for sub in stmt.body:
                    if isinstance(sub, ast.AnnAssign) and isinstance(
                        sub.target, ast.Name
                    ):
                        fields.append(
                            (sub.target.id, self.convert_type(sub.annotation))
                        )
                    if isinstance(sub, ast.FunctionDef):
                        methods.append(sub)
                base = stmt.bases[0].id if stmt.bases else None
                self.dataclasses.add(stmt.name)
                if base:
                    self.unions.setdefault(base, []).append((stmt.name, fields))
                else:
                    self.structs[stmt.name] = (fields, methods)

        # emit structs
        for name, (fields, methods) in self.structs.items():
            self.emit(f"type {name} {{")
            self.indent += 1
            for n, t in fields:
                self.emit(f"{n}: {t}")
            for m in methods:
                args = [
                    f"{a.arg}: {self.convert_type(a.annotation)}" for a in m.args.args[1:]
                ]
                ret = self.convert_type(m.returns)
                self.emit(f"fun {m.name}({', '.join(args)}): {ret} {{")
                self.indent += 1
                for st in m.body:
                    self.visit(st)
                self.indent -= 1
                self.emit("}")
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
            if isinstance(stmt, ast.FunctionDef) and stmt.name in {"_get", "_fetch", "_sort_key", "_load", "_save"}:
                continue
            if isinstance(stmt, ast.FunctionDef) and stmt.name == "main":
                for sub in stmt.body:
                    if isinstance(sub, ast.Global):
                        continue
                    if (
                        isinstance(sub, ast.Expr)
                        and isinstance(sub.value, ast.Call)
                        and isinstance(sub.value.func, ast.Name)
                        and sub.value.func.id.startswith("test_")
                    ):
                        continue
                    self.visit(sub)
                continue
            if isinstance(stmt, ast.FunctionDef):
                self.visit(stmt)
                continue
            if isinstance(stmt, ast.ClassDef):
                continue
            if isinstance(stmt, ast.Assign):
                self.visit(stmt)

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
        if node.name.startswith("test_"):
            name = node.name[5:]
            self.emit(f'test "{name}" {{')
            self.indent += 1
            for stmt in node.body:
                if isinstance(stmt, ast.Global):
                    continue
                self.visit(stmt)
            self.indent -= 1
            self.emit("}")
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
            if isinstance(stmt, ast.Global):
                continue
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

    def visit_Assert(self, node: ast.Assert) -> None:
        expr = self.convert_expr(node.test)
        self.emit(f"expect {expr}")

    def visit_Assign(self, node: ast.Assign) -> None:
        if len(node.targets) != 1:
            return
        target = node.targets[0]
        if isinstance(target, ast.Name):
            name = self.name_map.get(target.id, target.id)
            if isinstance(node.value, ast.Constant) and node.value.value is None:
                return
            if (
                isinstance(node.value, ast.Call)
                and isinstance(node.value.func, ast.Name)
                and node.value.func.id == "TypeVar"
            ):
                return
            if (
                isinstance(node.value, ast.Call)
                and isinstance(node.value.func, ast.Name)
                and node.value.func.id in self.dataclasses
                and not node.value.args
                and len(node.value.keywords) == 1
                and node.value.keywords[0].arg is None
                and isinstance(node.value.keywords[0].value, ast.Call)
                and isinstance(node.value.keywords[0].value.func, ast.Name)
                and node.value.keywords[0].value.func.id == "_fetch"
            ):
                typ = node.value.func.id
                fetch_expr = self.convert_expr(node.value.keywords[0].value)
                self.seen_assigns.add(name)
                self.assign_values[name] = f"({fetch_expr}) as {typ}"
                var_kw = "var" if name == "next" else "let"
                self.emit(f"{var_kw} {name}: {typ} = ({fetch_expr}) as {typ}")
                return
            expr = self.convert_expr(node.value)
            if name in self.seen_assigns:
                if self.assign_values.get(name) == expr:
                    return
                self.assign_values[name] = expr
                self.emit(f"{name} = {expr}")
                return
            self.seen_assigns.add(name)
            self.assign_values[name] = expr
            var_kw = "var" if name == "next" else "let"
            self.emit(f"{var_kw} {name} = {expr}")
            return
        if isinstance(target, ast.Attribute) and isinstance(target.value, ast.Name) and target.value.id == "self":
            self.emit(f"{target.attr} = {self.convert_expr(node.value)}")
            return

    def visit_Pass(self, node: ast.Pass) -> None:
        pass

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
    conv = Converter(src)
    try:
        conv.visit(tree)
    except ConversionError as e:
        lines = src.splitlines()
        start = max(e.lineno - 2, 0)
        end = min(e.lineno + 1, len(lines))
        context = "\n".join(f"{i + 1}: {lines[i]}" for i in range(start, end))
        raise ConversionError(
            f"{e} at line {e.lineno}: {e.line}\n{context}", e.lineno, e.line
        )
    return "\n".join(conv.lines) + ("\n" if conv.lines else "")


def main():
    ap = argparse.ArgumentParser(description="Convert a subset of Python to Mochi")
    ap.add_argument("file")
    ap.add_argument("-o", "--out")
    args = ap.parse_args()
    try:
        code = convert(args.file)
    except ConversionError as e:
        print(str(e), file=sys.stderr)
        raise SystemExit(1)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as f:
            f.write(code)
    else:
        print(code, end="")


if __name__ == "__main__":
    main()
