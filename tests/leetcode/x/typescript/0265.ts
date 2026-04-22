import * as fs from "fs";

function solve(costs: number[][]): number {
  if (costs.length === 0) return 0;
  let prev = costs[0].slice();
  for (let r = 1; r < costs.length; r++) {
    let min1 = Number.MAX_SAFE_INTEGER;
    let min2 = Number.MAX_SAFE_INTEGER;
    let idx1 = -1;
    for (let i = 0; i < prev.length; i++) {
      const v = prev[i];
      if (v < min1) {
        min2 = min1;
        min1 = v;
        idx1 = i;
      } else if (v < min2) {
        min2 = v;
      }
    }
    const cur = new Array(prev.length).fill(0);
    for (let i = 0; i < prev.length; i++) {
      cur[i] = costs[r][i] + (i === idx1 ? min2 : min1);
    }
    prev = cur;
  }
  return Math.min(...prev);
}

const toks = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(toks[idx++]);
    const k = Number(toks[idx++]);
    const costs: number[][] = Array.from({ length: n }, () => Array(k).fill(0));
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < k; j++) costs[i][j] = Number(toks[idx++]);
    }
    out.push(String(solve(costs)));
  }
  process.stdout.write(out.join("\n"));
}
