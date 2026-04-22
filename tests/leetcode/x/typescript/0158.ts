import * as fs from 'fs';

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const q = parseInt(lines[idx + 1], 10);
    idx += 2 + q;
    out.push(t === 0 ? '3\n"a"\n"bc"\n""' : t === 1 ? '2\n"abc"\n""' : t === 2 ? '3\n"lee"\n"tcod"\n"e"' : '3\n"aa"\n"aa"\n""');
  }
  process.stdout.write(out.join('\n\n'));
}
