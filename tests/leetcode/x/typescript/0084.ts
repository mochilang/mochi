import * as fs from 'fs';

function solve(a: number[]): number {
  let best = 0;
  for (let i = 0; i < a.length; i++) {
    let mn = a[i];
    for (let j = i; j < a.length; j++) {
      if (a[j] < mn) mn = a[j];
      const area = mn * (j - i + 1);
      if (area > best) best = area;
    }
  }
  return best;
}

const toks = fs.readFileSync(0, "utf8").split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(toks[idx++]);
    const a: number[] = [];
    for (let i = 0; i < n; i++) a.push(Number(toks[idx++]));
    out.push(String(solve(a)));
  }
  process.stdout.write(out.join("\n"));
}
