import * as fs from "fs";

function pick(nums: number[], k: number): number[] {
  let drop = nums.length - k;
  const stack: number[] = [];
  for (const x of nums) {
    while (drop > 0 && stack.length > 0 && stack[stack.length - 1] < x) {
      stack.pop();
      drop--;
    }
    stack.push(x);
  }
  return stack.slice(0, k);
}

function greater(a: number[], i: number, b: number[], j: number): boolean {
  while (i < a.length && j < b.length && a[i] === b[j]) {
    i++;
    j++;
  }
  return j === b.length || (i < a.length && a[i] > b[j]);
}

function merge(a: number[], b: number[]): number[] {
  const out: number[] = [];
  let i = 0, j = 0;
  while (i < a.length || j < b.length) {
    if (greater(a, i, b, j)) out.push(a[i++]);
    else out.push(b[j++]);
  }
  return out;
}

function maxNumber(nums1: number[], nums2: number[], k: number): number[] {
  let best: number[] = [];
  const start = Math.max(0, k - nums2.length);
  const end = Math.min(k, nums1.length);
  for (let take = start; take <= end; take++) {
    const cand = merge(pick(nums1, take), pick(nums2, k - take));
    if (greater(cand, 0, best, 0)) best = cand;
  }
  return best;
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
    const n1 = data[idx++];
    const nums1 = data.slice(idx, idx + n1);
    idx += n1;
    const n2 = data[idx++];
    const nums2 = data.slice(idx, idx + n2);
    idx += n2;
    const k = data[idx++];
    out.push(fmtList(maxNumber(nums1, nums2, k)));
  }
  process.stdout.write(out.join("\n\n"));
}
