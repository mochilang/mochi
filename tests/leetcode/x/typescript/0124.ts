import * as fs from 'fs';

function solve(vals: number[], ok: boolean[]): number {
  let best = -1_000_000_000;
  function dfs(i: number): number {
    if (i >= vals.length || !ok[i]) return 0;
    const left = Math.max(0, dfs(2 * i + 1));
    const right = Math.max(0, dfs(2 * i + 2));
    best = Math.max(best, vals[i] + left + right);
    return vals[i] + Math.max(left, right);
  }
  dfs(0);
  return best;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/).filter(s => s.length > 0);
if (lines.length > 0) {
  const tc = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const n = parseInt(lines[idx++], 10);
    const vals = Array(n).fill(0);
    const ok = Array(n).fill(false);
    for (let i = 0; i < n; i++) {
      const tok = lines[idx++];
      if (tok !== 'null') { ok[i] = true; vals[i] = parseInt(tok, 10); }
    }
    out.push(String(solve(vals, ok)));
  }
  process.stdout.write(out.join('\n'));
}
