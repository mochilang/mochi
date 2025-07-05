const fs = require('fs');
const luaparse = require('luaparse');

let src = '';
process.stdin.on('data', chunk => src += chunk);
process.stdin.on('end', () => {
  const ast = luaparse.parse(src, { locations: true, ranges: true });
  process.stdout.write(JSON.stringify(ast));
});
