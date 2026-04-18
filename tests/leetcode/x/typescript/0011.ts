import * as fs from 'fs';

function maxArea(h: number[]): number {
  let left = 0, right = h.length - 1, best = 0;
  while (left < right) {
    const height = Math.min(h[left], h[right]);
    best = Math.max(best, (right - left) * height);
    if (h[left] < h[right]) left++; else right--;
  }
  return best;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const t = parseInt(lines[0].trim(), 10);
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = parseInt(lines[idx++].trim(), 10);
    const h: number[] = [];
    for (let i = 0; i < n; i++) h.push(parseInt(lines[idx++].trim(), 10));
    out.push(String(maxArea(h)));
  }
  process.stdout.write(out.join('\n'));
}
