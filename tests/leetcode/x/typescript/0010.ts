import * as fs from 'fs';

function matchAt(s: string, p: string, i: number, j: number): boolean {
  if (j >= p.length) return i >= s.length;
  const first = i < s.length && (p[j] === '.' || s[i] === p[j]);
  if (j + 1 < p.length && p[j + 1] === '*') {
    return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j));
  }
  return first && matchAt(s, p, i + 1, j + 1);
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  const t = Number(lines[0].trim());
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const s = idx < lines.length ? lines[idx] : '';
    idx += 1;
    const p = idx < lines.length ? lines[idx] : '';
    idx += 1;
    out.push(matchAt(s, p, 0, 0) ? 'true' : 'false');
  }
  process.stdout.write(out.join('\n'));
}
