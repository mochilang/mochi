import * as fs from 'fs';

function solve(tri: number[][]): number {
  const dp = tri[tri.length - 1].slice();
  for (let i = tri.length - 2; i >= 0; i--) for (let j = 0; j <= i; j++) dp[j] = tri[i][j] + Math.min(dp[j], dp[j + 1]);
  return dp[0];
}

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rows = Number(toks[idx++]);
    const tri: number[][] = [];
    for (let r = 1; r <= rows; r++) {
      const row: number[] = [];
      for (let j = 0; j < r; j++) row.push(Number(toks[idx++]));
      tri.push(row);
    }
    out.push(String(solve(tri)));
  }
  process.stdout.write(out.join('\n'));
}
