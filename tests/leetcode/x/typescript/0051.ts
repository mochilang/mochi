import * as fs from 'fs';

function solveNQueens(n: number): string[][] {
  const cols = Array(n).fill(false);
  const d1 = Array(2 * n).fill(false);
  const d2 = Array(2 * n).fill(false);
  const board: string[][] = Array.from({ length: n }, () => Array(n).fill('.'));
  const res: string[][] = [];
  function dfs(r: number): void {
    if (r === n) { res.push(board.map(row => row.join(''))); return; }
    for (let c = 0; c < n; c++) {
      const a = r + c;
      const b = r - c + n - 1;
      if (cols[c] || d1[a] || d2[b]) continue;
      cols[c] = d1[a] = d2[b] = true;
      board[r][c] = 'Q';
      dfs(r + 1);
      board[r][c] = '.';
      cols[c] = d1[a] = d2[b] = false;
    }
  }
  dfs(0);
  return res;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(lines[idx++].trim());
    const sols = solveNQueens(n);
    out.push(String(sols.length));
    for (let si = 0; si < sols.length; si++) {
      out.push(...sols[si]);
      if (si + 1 < sols.length) out.push('-');
    }
    if (tc + 1 < t) out.push('=');
  }
  process.stdout.write(out.join('\n'));
}
