const fs = require('fs');
const ts = require('typescript');

const file = process.argv[2];
if (!file) {
  console.error('usage: ast.js <file>');
  process.exit(1);
}

const source = fs.readFileSync(file, 'utf8');
const sf = ts.createSourceFile(file, source, ts.ScriptTarget.Latest, true);
const program = { funcs: [], calls: [] };

for (const stmt of sf.statements) {
  if (ts.isFunctionDeclaration(stmt) && stmt.body) {
    const fn = {
      name: stmt.name.text,
      params: stmt.parameters.map(p => p.name.getText()),
      body: []
    };
    for (const s of stmt.body.statements) {
      if (ts.isReturnStatement(s)) {
        fn.body.push({ kind: 'return', expr: exprToObj(s.expression) });
      } else if (ts.isExpressionStatement(s) && ts.isCallExpression(s.expression) && isConsoleLog(s.expression)) {
        fn.body.push({ kind: 'print', expr: exprToObj(s.expression.arguments[0]) });
      } else {
        unsupported(s);
      }
    }
    program.funcs.push(fn);
  } else if (ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression)) {
    if (stmt.expression.arguments.length === 0) {
      program.calls.push({ name: stmt.expression.expression.getText() });
    } else {
      unsupported(stmt);
    }
  } else {
    unsupported(stmt);
  }
}

function isConsoleLog(call) {
  return ts.isPropertyAccessExpression(call.expression) &&
    call.expression.expression.getText() === 'console' &&
    call.expression.name.getText() === 'log';
}

function exprToObj(node) {
  if (!node) return null;
  if (ts.isNumericLiteral(node)) {
    return { kind: 'number', value: node.text };
  }
  if (ts.isIdentifier(node)) {
    return { kind: 'ident', name: node.text };
  }
  if (ts.isCallExpression(node)) {
    return { kind: 'call', name: node.expression.getText(), args: node.arguments.map(exprToObj) };
  }
  if (ts.isArrayLiteralExpression(node)) {
    return { kind: 'array', elems: node.elements.map(exprToObj) };
  }
  unsupported(node);
}

function unsupported(node) {
  console.error('unsupported syntax: ' + ts.SyntaxKind[node.kind]);
  process.exit(1);
}

console.log(JSON.stringify(program));
