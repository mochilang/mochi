import * as fs from "fs";

function solve(values: number[], target: number, k: number): number[] {
  let right = 0;
  while (right < values.length && values[right] < target) right++;
  let left = right - 1;
  const ans: number[] = [];
  while (ans.length < k) {
    if (left < 0) ans.push(values[right++]);
    else if (right >= values.length) ans.push(values[left--]);
    else if (Math.abs(values[left] - target) <= Math.abs(values[right] - target)) ans.push(values[left--]);
    else ans.push(values[right++]);
  }
  return ans;
}

const toks = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const blocks: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(toks[idx++]);
    const values = toks.slice(idx, idx + n).map(Number);
    idx += n;
    const target = Number(toks[idx++]);
    const k = Number(toks[idx++]);
    const ans = solve(values, target, k);
    blocks.push([String(ans.length), ...ans.map(String)].join("\n"));
  }
  process.stdout.write(blocks.join("\n\n"));
}
