import * as fs from "fs";

function longestIncreasingPath(matrix: number[][]): number {
  const rows = matrix.length, cols = matrix[0].length;
  const memo = Array.from({ length: rows }, () => Array(cols).fill(0));
  const dirs = [[1,0],[-1,0],[0,1],[0,-1]];
  function dfs(r: number, c: number): number {
    if (memo[r][c] !== 0) return memo[r][c];
    let best = 1;
    for (const [dr, dc] of dirs) {
      const nr = r + dr, nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr][nc] > matrix[r][c]) {
        best = Math.max(best, 1 + dfs(nr, nc));
      }
    }
    memo[r][c] = best;
    return best;
  }
  let ans = 0;
  for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) ans = Math.max(ans, dfs(r, c));
  return ans;
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let idx = 0; const t = data[idx++]; const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rows = data[idx++], cols = data[idx++];
    const m: number[][] = [];
    for (let r = 0; r < rows; r++) { m.push(data.slice(idx, idx + cols)); idx += cols; }
    out.push(String(longestIncreasingPath(m)));
  }
  process.stdout.write(out.join("\n\n"));
}
