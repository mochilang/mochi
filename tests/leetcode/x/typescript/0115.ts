import * as fs from 'fs';

function solve(s: string, t: string): number {
  const dp: number[] = Array(t.length + 1).fill(0);
  dp[0] = 1;
  for (const ch of s) {
    for (let j = t.length; j >= 1; j--) {
      if (ch === t[j - 1]) dp[j] += dp[j - 1];
    }
  }
  return dp[t.length];
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  const out: string[] = [];
  for (let i = 0; i < tc; i++) {
    out.push(String(solve(lines[1 + 2 * i], lines[2 + 2 * i])));
  }
  process.stdout.write(out.join('\n'));
}
