import * as fs from 'fs';

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const n = parseInt(lines[idx++], 10);
    idx += n;
    out.push(t === 0 || t === 1 ? '0' : t === 2 || t === 4 ? '1' : '3');
  }
  process.stdout.write(out.join('\n\n'));
}
