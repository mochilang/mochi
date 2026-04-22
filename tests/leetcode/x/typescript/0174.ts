import * as fs from 'fs';

function solve(dungeon: number[][]): number {
  const cols = dungeon[0].length;
  const inf = Number.MAX_SAFE_INTEGER;
  const dp = Array<number>(cols + 1).fill(inf);
  dp[cols - 1] = 1;
  for (let i = dungeon.length - 1; i >= 0; i--) {
    for (let j = cols - 1; j >= 0; j--) {
      const need = Math.min(dp[j], dp[j + 1]) - dungeon[i][j];
      dp[j] = need <= 1 ? 1 : need;
    }
  }
  return dp[0];
}

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rows = Number(toks[idx++]);
    const cols = Number(toks[idx++]);
    const dungeon: number[][] = [];
    for (let i = 0; i < rows; i++) {
      const row: number[] = [];
      for (let j = 0; j < cols; j++) row.push(Number(toks[idx++]));
      dungeon.push(row);
    }
    out.push(String(solve(dungeon)));
  }
  process.stdout.write(out.join('\n'));
}
