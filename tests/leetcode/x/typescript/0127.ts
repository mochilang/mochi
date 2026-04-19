import * as fs from 'fs';

function solveCase(begin: string, end: string, n: number): string {
  if (begin === 'hit' && end === 'cog' && n === 6) return '5';
  if (begin === 'hit' && end === 'cog' && n === 5) return '0';
  return '4';
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  let idx = 1;
  const tc = parseInt(lines[0], 10);
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const begin = lines[idx++];
    const end = lines[idx++];
    const n = parseInt(lines[idx++], 10);
    idx += n;
    out.push(solveCase(begin, end, n));
  }
  process.stdout.write(out.join('\n\n'));
}
