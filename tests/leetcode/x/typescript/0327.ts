import * as fs from "fs";

function countRangeSum(nums: number[], lower: number, upper: number): number {
  const pref: number[] = [0];
  for (const x of nums) pref.push(pref[pref.length - 1] + x);
  const tmp = pref.slice();
  function sort(lo: number, hi: number): number {
    if (hi - lo <= 1) return 0;
    const mid = Math.floor((lo + hi) / 2);
    let ans = sort(lo, mid) + sort(mid, hi);
    let left = lo, right = lo;
    for (let r = mid; r < hi; r++) {
      while (left < mid && pref[left] < pref[r] - upper) left++;
      while (right < mid && pref[right] <= pref[r] - lower) right++;
      ans += right - left;
    }
    let i = lo, j = mid, k = lo;
    while (i < mid && j < hi) tmp[k++] = pref[i] <= pref[j] ? pref[i++] : pref[j++];
    while (i < mid) tmp[k++] = pref[i++];
    while (j < hi) tmp[k++] = pref[j++];
    for (let p = lo; p < hi; p++) pref[p] = tmp[p];
    return ans;
  }
  return sort(0, pref.length);
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let idx = 0;
  const t = data[idx++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = data[idx++];
    const nums = data.slice(idx, idx + n); idx += n;
    const lower = data[idx++], upper = data[idx++];
    out.push(String(countRangeSum(nums, lower, upper)));
  }
  process.stdout.write(out.join("\n\n"));
}
