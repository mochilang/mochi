const fs = require('fs');
const path = require('path');
const tsPath = process.env.TS_LIB || 'typescript';
const ts = require(tsPath);

function pos(source, p) {
  const lc = source.getLineAndCharacterOfPosition(p);
  return { line: lc.line + 1, col: lc.character };
}

function convert(node, source) {
  const start = pos(source, node.getStart(source));
  const end = pos(source, node.end);
  const obj = {
    kind: ts.SyntaxKind[node.kind],
    start: start.line,
    startCol: start.col,
    end: end.line,
    endCol: end.col,
  };

  if (ts.isIdentifier(node)) {
    obj.name = node.text;
  } else if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) {
    obj.value = node.text;
  } else if (ts.isNumericLiteral(node)) {
    obj.value = Number(node.text);
  } else if (ts.isBooleanLiteral(node)) {
    obj.value = node.kind === ts.SyntaxKind.TrueKeyword;
  }

  const children = [];
  ts.forEachChild(node, (c) => {
    children.push(convert(c, source));
  });
  if (children.length) {
    obj.children = children;
  }
  return obj;
}

const file = process.argv[2];
const src = fs.readFileSync(file, 'utf8');
const source = ts.createSourceFile('input.ts', src, ts.ScriptTarget.Latest, true, ts.ScriptKind.TS);
console.log(JSON.stringify(convert(source, source)));
