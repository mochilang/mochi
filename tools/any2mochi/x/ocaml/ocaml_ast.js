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

const prog = { funcs: [], prints: [], types: [], vars: [] };
for (let i = 0; i < tree.rootNode.namedChildren.length; i++) {
  const child = tree.rootNode.namedChildren[i];
  if (child.type === 'ERROR' && child.text.trim().startsWith('type ')) {
    const m = /^type\s+([A-Za-z0-9_']+)\s*=\s*$/.exec(child.text.trim());
    const next = child.nextNamedSibling;
    if (m && next && next.type === 'expression_item') {
      const rec = next.namedChildren.find(n => n.type === 'record_expression');
      if (rec) {
        const fields = [];
        for (const fld of rec.namedChildren) {
          if (fld.type !== 'field_expression') continue;
          const path = fld.namedChildren.find(n => n.type === 'field_path');
          const nameNode = path ? path.namedChildren.find(n => n.type === 'field_name') : null;
          const name = nameNode ? text(nameNode) : '';
          const tpath = fld.namedChildren.find(n => n.type === 'type_constructor_path');
          let ftype = '';
          if (tpath) {
            const tc = tpath.namedChildren.find(n => n.type === 'type_constructor');
            if (tc) ftype = text(tc);
          }
          fields.push({ name, type: ftype, line: fld.startPosition.row + 1 });
        }
        prog.types.push({ name: m[1], fields, line: child.startPosition.row + 1 });
        i++; // skip following expression_item
        continue;
      }
    }
  }
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
    const line = child.startPosition.row + 1;
    if (params.length > 0) {
      prog.funcs.push({ name, params, body: body.trim(), line });
    } else if (name) {
      prog.vars.push({ name, expr: body.trim().replace(/;$/, ''), line });
    } else {
      prog.prints.push({ expr: body.trim().replace(/;$/, ''), line });
    }
  } else if (child.type === 'expression_item') {
    prog.prints.push({ expr: text(child).trim().replace(/;$/, ''), line: child.startPosition.row + 1 });
  } else if (child.type === 'type_definition') {
    for (const bind of child.namedChildren) {
      if (bind.type !== 'type_binding') continue;
      const nameNode = bind.namedChildren.find(c => c.type === 'type_constructor');
      const name = nameNode ? text(nameNode) : '';
      const rec = bind.namedChildren.find(c => c.type === 'record_declaration');
      if (!rec) continue;
      const fields = [];
      for (const fld of rec.namedChildren) {
        if (fld.type !== 'field_expression' && fld.type !== 'field_declaration') continue;
        const fnameNode = fld.namedChildren.find(n => n.type === 'field_name');
        const fname = fnameNode ? text(fnameNode) : '';
        const tpath = fld.namedChildren.find(n => n.type === 'type_constructor_path');
        let ftype = '';
        if (tpath) {
          const tc = tpath.namedChildren.find(n => n.type === 'type_constructor');
          if (tc) ftype = text(tc);
        }
        fields.push({ name: fname, type: ftype, line: fld.startPosition.row + 1 });
      }
      prog.types.push({ name, fields, line: bind.startPosition.row + 1 });
    }
  }
}
console.log(JSON.stringify(prog));
