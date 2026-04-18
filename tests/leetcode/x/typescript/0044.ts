import * as fs from 'fs';

function isMatch(s: string, p: string): boolean {
  let i = 0;
  let j = 0;
  let star = -1;
  let match = 0;
  while (i < s.length) {
    if (j < p.length && (p[j] === '?' || p[j] === s[i])) { i += 1; j += 1; }
    else if (j < p.length && p[j] === '*') { star = j; match = i; j += 1; }
    else if (star !== -1) { j = star + 1; match += 1; i = match; }
    else return false;
  }
  while (j < p.length && p[j] === '*') j += 1;
  return j === p.length;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(lines[idx++].trim());
    const s = n > 0 ? lines[idx++] : '';
    const m = Number(lines[idx++].trim());
    const p = m > 0 ? lines[idx++] : '';
    out.push(isMatch(s, p) ? 'true' : 'false');
  }
  process.stdout.write(out.join('\n'));
}
