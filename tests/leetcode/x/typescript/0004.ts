import * as fs from 'fs';

function median(a: number[], b: number[]): number {
  const m = a.concat(b).sort((x, y) => x - y);
  if (m.length % 2 === 1) return m[Math.floor(m.length / 2)];
  return (m[m.length / 2 - 1] + m[m.length / 2]) / 2.0;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const t = parseInt(lines[0].trim(), 10);
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = parseInt(lines[idx++].trim(), 10);
    const a: number[] = [];
    for (let i = 0; i < n; i++) a.push(parseInt(lines[idx++].trim(), 10));
    const m = parseInt(lines[idx++].trim(), 10);
    const b: number[] = [];
    for (let i = 0; i < m; i++) b.push(parseInt(lines[idx++].trim(), 10));
    out.push(median(a,b).toFixed(1));
  }
  process.stdout.write(out.join('\n'));
}
