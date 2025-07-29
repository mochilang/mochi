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
  value?: string;
  fields?: TSField[];
  alias?: string;
  variants?: string[];
  expr?: string;
  iter?: string;
  list?: string;
  startVal?: string;
  endVal?: string;
  cond?: string;
  start?: number;
  startCol?: number;
  end?: number;
  endCol?: number;
  snippet?: string;
  startOff?: number;
  endOff?: number;
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
          init &&
          (ts.isArrowFunction(init) || ts.isFunctionExpression(init))
        ) {
          const params: TSParam[] = init.parameters.map((p) => ({
            name: p.name.getText(source),
            typ: p.type ? p.type.getText(source) : "",
          }));
          const rt = init.type ? init.type.getText(source) : "";
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
          const typ = d.type ? d.type.getText(source) : "";
          const value = init ? init.getText(source) : "";
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
            value,
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
        typ: p.type ? p.type.getText(source) : "",
      }));
      const rt = stmt.type ? stmt.type.getText(source) : "";
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
        if (ts.isPropertyDeclaration(mem) || ts.isPropertySignature(mem)) {
          fields.push({
            name: mem.name.getText(source),
            typ: mem.type ? mem.type.getText(source) : "",
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
              typ: m.type ? m.type.getText(source) : "",
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
        const alias = tn.getText(source);
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
    } else if (ts.isForOfStatement(stmt)) {
      let iter = stmt.initializer.getText(source);
      if (ts.isVariableDeclarationList(stmt.initializer)) {
        const d = stmt.initializer.declarations[0];
        iter = d.name.getText(source);
      }
      let list = stmt.expression.getText(source);
      let body = stmt.statement.getText(source);
      if (ts.isBlock(stmt.statement)) body = body.slice(1, -1);
      decls.push({
        kind: "forof",
        node: ts.SyntaxKind[stmt.kind],
        iter,
        list,
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
    } else if (ts.isForInStatement(stmt)) {
      let iter = stmt.initializer.getText(source);
      if (ts.isVariableDeclarationList(stmt.initializer)) {
        const d = stmt.initializer.declarations[0];
        iter = d.name.getText(source);
      }
      let list = stmt.expression.getText(source);
      let body = stmt.statement.getText(source);
      if (ts.isBlock(stmt.statement)) body = body.slice(1, -1);
      decls.push({
        kind: "forin",
        node: ts.SyntaxKind[stmt.kind],
        iter,
        list,
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
    } else if (ts.isForStatement(stmt)) {
      let iter = "";
      let startVal = "";
      if (stmt.initializer && ts.isVariableDeclarationList(stmt.initializer)) {
        const d = stmt.initializer.declarations[0];
        iter = d.name.getText(source);
        startVal = d.initializer ? d.initializer.getText(source) : "";
      }
      let endVal = "";
      if (
        stmt.condition &&
        ts.isBinaryExpression(stmt.condition) &&
        stmt.condition.left.getText(source) === iter &&
        stmt.condition.operatorToken.kind === ts.SyntaxKind.LessThanToken
      ) {
        endVal = stmt.condition.right.getText(source);
      }
      let body = stmt.statement.getText(source);
      if (ts.isBlock(stmt.statement)) body = body.slice(1, -1);
      decls.push({
        kind: "for",
        node: ts.SyntaxKind[stmt.kind],
        iter,
        startVal,
        endVal,
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
    } else if (ts.isWhileStatement(stmt)) {
      const cond = stmt.expression.getText(source);
      let body = stmt.statement.getText(source);
      if (ts.isBlock(stmt.statement)) body = body.slice(1, -1);
      decls.push({
        kind: "while",
        node: ts.SyntaxKind[stmt.kind],
        cond,
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
    } else if (ts.isIfStatement(stmt)) {
      const cond = stmt.expression.getText(source);
      let body = stmt.thenStatement.getText(source);
      if (ts.isBlock(stmt.thenStatement)) body = body.slice(1, -1);
      let elsePart = "";
      if (stmt.elseStatement) {
        elsePart = stmt.elseStatement.getText(source);
        if (ts.isBlock(stmt.elseStatement)) {
          elsePart = elsePart.slice(1, -1);
        }
      }
      decls.push({
        kind: "if",
        node: ts.SyntaxKind[stmt.kind],
        cond,
        body,
        else: elsePart,
        start: start.line,
        startCol: start.col,
        end: end.line,
        endCol: end.col,
        snippet,
        startOff: stmt.getStart(source),
        endOff: stmt.end,
        doc,
      });
    } else if (ts.isExpressionStatement(stmt)) {
      decls.push({
        kind: "expr",
        node: ts.SyntaxKind[stmt.kind],
        expr: stmt.expression.getText(source),
        start: start.line,
        startCol: start.col,
        end: end.line,
        endCol: end.col,
        snippet,
        startOff: stmt.getStart(source),
        endOff: stmt.end,
        doc,
      });
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
