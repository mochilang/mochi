import * as fs from "fs";

function maxCoins(nums: number[]): number {
  const vals = [1, ...nums, 1];
  const n = vals.length;
  const dp = Array.from({ length: n }, () => Array(n).fill(0));
  for (let length = 2; length < n; length++) {
    for (let left = 0; left + length < n; left++) {
      const right = left + length;
      for (let k = left + 1; k < right; k++) {
        dp[left][right] = Math.max(dp[left][right], dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right]);
      }
    }
  }
  return dp[0][n - 1];
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let idx = 0;
  const t = data[idx++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = data[idx++];
    const nums = data.slice(idx, idx + n);
    idx += n;
    out.push(String(maxCoins(nums)));
  }
  process.stdout.write(out.join("\n\n"));
}
