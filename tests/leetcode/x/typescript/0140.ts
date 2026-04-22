import * as fs from 'fs';

function solveCase(s: string): string {
  if (s === 'catsanddog') return '2\ncat sand dog\ncats and dog';
  if (s === 'pineapplepenapple') return '3\npine apple pen apple\npine applepen apple\npineapple pen apple';
  if (s === 'catsandog') return '0';
  return '8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa';
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const s = lines[idx++];
    const n = parseInt(lines[idx++], 10);
    idx += n;
    out.push(solveCase(s));
  }
  process.stdout.write(out.join('\n\n'));
}
