import * as fs from 'fs';

function solve(s1: string, s2: string, s3: string): boolean {
  const m = s1.length, n = s2.length;
  if (m + n !== s3.length) return false;
  const dp: boolean[][] = Array.from({ length: m + 1 }, () => Array(n + 1).fill(false));
  dp[0][0] = true;
  for (let i = 0; i <= m; i++) {
    for (let j = 0; j <= n; j++) {
      if (i > 0 && dp[i - 1][j] && s1[i - 1] === s3[i + j - 1]) dp[i][j] = true;
      if (j > 0 && dp[i][j - 1] && s2[j - 1] === s3[i + j - 1]) dp[i][j] = true;
    }
  }
  return dp[m][n];
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(solve(lines[1 + 3*i], lines[2 + 3*i], lines[3 + 3*i]) ? 'true' : 'false');
  process.stdout.write(out.join('\n'));
}
