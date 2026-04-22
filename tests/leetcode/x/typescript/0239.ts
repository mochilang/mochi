import * as fs from "fs";

function solve(nums: number[], k: number): number[] {
  const dq: number[] = [];
  const ans: number[] = [];
  for (let i = 0; i < nums.length; i++) {
    while (dq.length > 0 && dq[0] <= i - k) dq.shift();
    while (dq.length > 0 && nums[dq[dq.length - 1]] <= nums[i]) dq.pop();
    dq.push(i);
    if (i >= k - 1) ans.push(nums[dq[0]]);
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
    const nums = toks.slice(idx, idx + n).map(Number);
    idx += n;
    const k = Number(toks[idx++]);
    const ans = solve(nums, k);
    blocks.push([String(ans.length), ...ans.map(String)].join("\n"));
  }
  process.stdout.write(blocks.join("\n\n"));
}
