const Parser = require('tree-sitter');
const Cs = require('tree-sitter-c-sharp');

const parser = new Parser();
parser.setLanguage(Cs);

let src='';
process.stdin.setEncoding('utf8');
process.stdin.on('data', d => src += d);
process.stdin.on('end', () => {
  const tree = parser.parse(src);
  function toObj(node) {
    let obj = {type: node.type, text: node.text};
    if (node.namedChildCount > 0) {
      obj.children = [];
      for (let i = 0; i < node.namedChildCount; i++) {
        obj.children.push(toObj(node.namedChild(i)));
      }
    }
    return obj;
  }
  console.log(JSON.stringify(toObj(tree.rootNode)));
});
