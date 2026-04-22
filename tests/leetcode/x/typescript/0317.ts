import * as fs from "fs";

function shortestDistance(grid: number[][]): number {
  const rows = grid.length;
  const cols = grid[0].length;
  const dist = Array.from({ length: rows }, () => Array(cols).fill(0));
  const reach = Array.from({ length: rows }, () => Array(cols).fill(0));
  let buildings = 0;
  for (let sr = 0; sr < rows; sr++) {
    for (let sc = 0; sc < cols; sc++) {
      if (grid[sr][sc] !== 1) continue;
      buildings++;
      const seen = Array.from({ length: rows }, () => Array(cols).fill(false));
      const q: [number, number, number][] = [[sr, sc, 0]];
      seen[sr][sc] = true;
      for (let head = 0; head < q.length; head++) {
        const [r, c, d] = q[head];
        for (const [dr, dc] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
          const nr = r + dr;
          const nc = c + dc;
          if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !seen[nr][nc]) {
            seen[nr][nc] = true;
            if (grid[nr][nc] === 0) {
              dist[nr][nc] += d + 1;
              reach[nr][nc] += 1;
              q.push([nr, nc, d + 1]);
            }
          }
        }
      }
    }
  }
  let ans = -1;
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (grid[r][c] === 0 && reach[r][c] === buildings) {
        if (ans === -1 || dist[r][c] < ans) ans = dist[r][c];
      }
    }
  }
  return ans;
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let pos = 0;
  const t = data[pos++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rows = data[pos++];
    const cols = data[pos++];
    const grid: number[][] = [];
    for (let i = 0; i < rows; i++) {
      grid.push(data.slice(pos, pos + cols));
      pos += cols;
    }
    out.push(String(shortestDistance(grid)));
  }
  process.stdout.write(out.join("\n\n"));
}
