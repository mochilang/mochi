import * as ts from "npm:typescript";

interface TSParam {
  name: string;
  typ: string;
}

interface TSField {
  name: string;
  typ: string;
}

interface TSDecl {
  kind: string;
  name: string;
  node?: string;
  params?: TSParam[];
  ret?: string;
  body?: string;
  fields?: TSField[];
  alias?: string;
  variants?: string[];
  start?: number;
  startCol?: number;
  end?: number;
  endCol?: number;
  snippet?: string;
  startOff?: number;
  endOff?: number;
}

function tsToMochiType(t: string): string {
  t = t.trim();
  if (t.includes("|")) {
    const parts = t
      .split("|")
      .map((p) => p.trim())
      .filter((p) => p !== "null" && p !== "undefined")
      .map(tsToMochiType)
      .filter(Boolean);
    if (parts.length === 1) return parts[0];
    if (parts.length > 1) return "any";
    return "";
  }
  switch (t) {
    case "":
    case "any":
    case "unknown":
    case "object":
      return "";
    case "number":
      return "int";
    case "string":
      return "string";
    case "boolean":
      return "bool";
    case "void":
    case "undefined":
    case "null":
      return "";
  }
  if (t.endsWith("[]")) {
    const inner = tsToMochiType(t.slice(0, -2)) || "any";
    return `list<${inner}>`;
  }
  if (t.startsWith("Array<") && t.endsWith(">")) {
    const inner = tsToMochiType(t.slice(6, -1)) || "any";
    return `list<${inner}>`;
  }
  if (t.startsWith("Record<") && t.endsWith(">")) {
    const [k, v] = t.slice(7, -1).split(/\s*,\s*/);
    const key = tsToMochiType(k || "any") || "any";
    const val = tsToMochiType(v || "any") || "any";
    return `map<${key},${val}>`;
  }
  return t;
}

function pos(source: ts.SourceFile, p: number) {
  const lc = source.getLineAndCharacterOfPosition(p);
  return { line: lc.line + 1, col: lc.character };
}

function parse(src: string): TSDecl[] {
  const source = ts.createSourceFile(
    "input.ts",
    src,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS,
  );
  const decls: TSDecl[] = [];

  source.statements.forEach((stmt) => {
    const start = pos(source, stmt.getStart(source));
    const end = pos(source, stmt.end);
    const snippet = stmt.getText(source);
    const docRanges = ts.getLeadingCommentRanges(src, stmt.pos) ?? [];
    const doc = docRanges.map((r) => src.slice(r.pos, r.end)).join("\n");

    if (ts.isVariableStatement(stmt)) {
      stmt.declarationList.declarations.forEach((d) => {
        const name = d.name.getText(source);
        const init = d.initializer;
        if (
          init && (ts.isArrowFunction(init) || ts.isFunctionExpression(init))
        ) {
          const params: TSParam[] = init.parameters.map((p) => ({
            name: p.name.getText(source),
            typ: tsToMochiType(p.type ? p.type.getText(source) : ""),
          }));
          const rt = tsToMochiType(init.type ? init.type.getText(source) : "");
          let body = "";
          if (ts.isBlock(init.body)) {
            body = init.body.getText(source).slice(1, -1);
          }
          decls.push({
            kind: "funcvar",
            name,
            node: ts.SyntaxKind[init.kind],
            params,
            ret: rt,
            body,
            start: start.line,
            startCol: start.col,
            end: end.line,
            endCol: end.col,
            snippet,
            startOff: stmt.getStart(source),
            endOff: stmt.end,
            doc,
          });
        } else {
          const typ = tsToMochiType(d.type ? d.type.getText(source) : "");
          decls.push({
            kind: "var",
            name,
            node: ts.SyntaxKind[d.kind],
            start: start.line,
            startCol: start.col,
            end: end.line,
            endCol: end.col,
            snippet,
            ret: typ,
            startOff: stmt.getStart(source),
            endOff: stmt.end,
            doc,
          });
        }
      });
    } else if (ts.isFunctionDeclaration(stmt) && stmt.name) {
      const name = stmt.name.getText(source);
      const params: TSParam[] = stmt.parameters.map((p) => ({
        name: p.name.getText(source),
        typ: tsToMochiType(p.type ? p.type.getText(source) : ""),
      }));
      const rt = tsToMochiType(stmt.type ? stmt.type.getText(source) : "");
      let body = "";
      if (stmt.body) body = stmt.body.getText(source).slice(1, -1);
      decls.push({
        kind: "func",
        node: ts.SyntaxKind[stmt.kind],
        name,
        start: start.line,
        startCol: start.col,
        end: end.line,
        endCol: end.col,
        snippet,
        params,
        ret: rt,
        body,
        startOff: stmt.getStart(source),
        endOff: stmt.end,
        doc,
      });
    } else if (ts.isEnumDeclaration(stmt)) {
      const variants = stmt.members.map((m) => m.name.getText(source));
      decls.push({
        kind: "enum",
        node: ts.SyntaxKind[stmt.kind],
        name: stmt.name.getText(source),
        start: start.line,
        startCol: start.col,
        end: end.line,
        endCol: end.col,
        snippet,
        variants,
        startOff: stmt.getStart(source),
        endOff: stmt.end,
        doc,
      });
    } else if (ts.isClassDeclaration(stmt) || ts.isInterfaceDeclaration(stmt)) {
      const fields: TSField[] = [];
      stmt.members.forEach((mem) => {
        if (
          ts.isPropertyDeclaration(mem) ||
          ts.isPropertySignature(mem)
        ) {
          fields.push({
            name: mem.name.getText(source),
            typ: tsToMochiType(mem.type ? mem.type.getText(source) : ""),
          });
        }
      });
      decls.push({
        kind: "type",
        node: ts.SyntaxKind[stmt.kind],
        name: stmt.name ? stmt.name.getText(source) : "",
        start: start.line,
        startCol: start.col,
        end: end.line,
        endCol: end.col,
        snippet,
        fields,
        startOff: stmt.getStart(source),
        endOff: stmt.end,
        doc,
      });
    } else if (ts.isTypeAliasDeclaration(stmt)) {
      const name = stmt.name.getText(source);
      const tn = stmt.type;
      if (tn && ts.isTypeLiteralNode(tn)) {
        const fields: TSField[] = [];
        tn.members.forEach((m) => {
          if (ts.isPropertySignature(m)) {
            fields.push({
              name: m.name.getText(source),
              typ: tsToMochiType(m.type ? m.type.getText(source) : ""),
            });
          }
        });
        decls.push({
          kind: "type",
          node: ts.SyntaxKind[stmt.kind],
          name,
          start: start.line,
          startCol: start.col,
          end: end.line,
          endCol: end.col,
          snippet,
          fields,
          startOff: stmt.getStart(source),
          endOff: stmt.end,
          doc,
        });
      } else if (tn) {
        const alias = tsToMochiType(tn.getText(source));
        decls.push({
          kind: "alias",
          node: ts.SyntaxKind[stmt.kind],
          name,
          start: start.line,
          startCol: start.col,
          end: end.line,
          endCol: end.col,
          snippet,
          alias,
          startOff: stmt.getStart(source),
          endOff: stmt.end,
          doc,
        });
      }
    }
  });

  return decls;
}

if (import.meta.main) {
  const file = Deno.args[0];
  const src = await Deno.readTextFile(file);
  console.log(JSON.stringify(parse(src)));
}

export default { parse };
