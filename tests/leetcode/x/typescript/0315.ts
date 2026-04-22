import * as fs from "fs";

function countSmaller(nums: number[]): number[] {
  const n = nums.length;
  const counts = Array(n).fill(0);
  const idx = Array.from({ length: n }, (_, i) => i);
  const tmp = Array(n).fill(0);

  function sort(lo: number, hi: number): void {
    if (hi - lo <= 1) return;
    const mid = (lo + hi) >> 1;
    sort(lo, mid);
    sort(mid, hi);
    let i = lo, j = mid, k = lo, moved = 0;
    while (i < mid && j < hi) {
      if (nums[idx[j]] < nums[idx[i]]) {
        tmp[k++] = idx[j++];
        moved++;
      } else {
        counts[idx[i]] += moved;
        tmp[k++] = idx[i++];
      }
    }
    while (i < mid) {
      counts[idx[i]] += moved;
      tmp[k++] = idx[i++];
    }
    while (j < hi) tmp[k++] = idx[j++];
    for (let p = lo; p < hi; p++) idx[p] = tmp[p];
  }

  sort(0, n);
  return counts;
}

function fmtList(a: number[]): string {
  return `[${a.join(",")}]`;
}

const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) {
  let pos = 0;
  const t = data[pos++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = data[pos++];
    const nums = data.slice(pos, pos + n);
    pos += n;
    out.push(fmtList(countSmaller(nums)));
  }
  process.stdout.write(out.join("\n\n"));
}
