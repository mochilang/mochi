const fs = require('fs');
const Parser = require('tree-sitter');
const Ocaml = require('tree-sitter-ocaml');

const file = process.argv[2];
if (!file) {
  console.error('usage: ocaml_ast.js <file>');
  process.exit(1);
}
const source = fs.readFileSync(file, 'utf8');
const parser = new Parser();
parser.setLanguage(Ocaml.ocaml);
const tree = parser.parse(source);
function text(node) { return source.slice(node.startIndex, node.endIndex); }

const prog = { funcs: [], prints: [] };
for (const child of tree.rootNode.namedChildren) {
  if (child.type === 'value_definition') {
    const binding = child.namedChildren.find(c => c.type === 'let_binding');
    if (!binding) continue;
    const nameNode = binding.namedChildren.find(c => c.type === 'value_name');
    const name = nameNode ? text(nameNode) : '';
    const params = [];
    for (const c of binding.namedChildren) {
      if (c.type === 'parameter') {
        const pat = c.namedChildren.find(n => n.type === 'value_pattern');
        if (pat) params.push(text(pat));
      }
    }
    const eqIdx = binding.children.findIndex(c => c.type === '=');
    let body = '';
    if (eqIdx !== -1 && eqIdx + 1 < binding.children.length) {
      const node = binding.children[eqIdx + 1];
      body = text({ startIndex: node.startIndex, endIndex: binding.endIndex });
    }
    if (params.length > 0) {
      prog.funcs.push({ name, params, body: body.trim() });
    } else {
      prog.prints.push(body.trim().replace(/;$/, ''));
    }
  } else if (child.type === 'expression_item') {
    prog.prints.push(text(child).trim().replace(/;$/, ''));
  }
}
console.log(JSON.stringify(prog));
