import * as fs from "fs";

function solve(m: number, n: number, positions: [number, number][]): number[] {
  const parent = new Map<number, number>();
  const rank = new Map<number, number>();
  const ans: number[] = [];
  let count = 0;

  function find(x: number): number {
    while (parent.get(x)! !== x) {
      parent.set(x, parent.get(parent.get(x)!)!);
      x = parent.get(x)!;
    }
    return x;
  }

  function union(a: number, b: number): boolean {
    let ra = find(a);
    let rb = find(b);
    if (ra === rb) return false;
    if (rank.get(ra)! < rank.get(rb)!) [ra, rb] = [rb, ra];
    parent.set(rb, ra);
    if (rank.get(ra)! === rank.get(rb)!) rank.set(ra, rank.get(ra)! + 1);
    return true;
  }

  for (const [r, c] of positions) {
    const idx = r * n + c;
    if (parent.has(idx)) {
      ans.push(count);
      continue;
    }
    parent.set(idx, idx);
    rank.set(idx, 0);
    count++;
    for (const [dr, dc] of [[1, 0], [-1, 0], [0, 1], [0, -1]]) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < m && nc >= 0 && nc < n) {
        const nei = nr * n + nc;
        if (parent.has(nei) && union(idx, nei)) count--;
      }
    }
    ans.push(count);
  }
  return ans;
}

function fmtList(a: number[]): string {
  return `[${a.join(",")}]`;
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let idx = 0;
  const t = data[idx++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const m = data[idx++], n = data[idx++], k = data[idx++];
    const positions: [number, number][] = [];
    for (let i = 0; i < k; i++) positions.push([data[idx++], data[idx++]]);
    out.push(fmtList(solve(m, n, positions)));
  }
  process.stdout.write(out.join("\n\n"));
}
