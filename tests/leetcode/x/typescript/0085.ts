import * as fs from 'fs';

function hist(h: number[]): number {
  let best = 0;
  for (let i = 0; i < h.length; i++) {
    let mn = h[i];
    for (let j = i; j < h.length; j++) {
      if (h[j] < mn) mn = h[j];
      const area = mn * (j - i + 1);
      if (area > best) best = area;
    }
  }
  return best;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  const t = Number(lines[0].trim());
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rc = lines[idx++].trim().split(/\s+/);
    const rows = Number(rc[0]), cols = Number(rc[1]);
    const h = Array(cols).fill(0);
    let best = 0;
    for (let r = 0; r < rows; r++) {
      const s = lines[idx++].trim();
      for (let c = 0; c < cols; c++) h[c] = s[c] === '1' ? h[c] + 1 : 0;
      best = Math.max(best, hist(h));
    }
    out.push(String(best));
  }
  process.stdout.write(out.join('\n'));
}
