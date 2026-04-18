import * as fs from 'fs';
function firstMissingPositive(nums: number[]): number {
  const n = nums.length;
  let i = 0;
  while (i < n) {
    const v = nums[i];
    if (v >= 1 && v <= n && nums[v - 1] !== v) {
      const tmp = nums[i];
      nums[i] = nums[v - 1];
      nums[v - 1] = tmp;
    } else {
      i += 1;
    }
  }
  for (let j = 0; j < n; j++) if (nums[j] !== j + 1) return j + 1;
  return n + 1;
}
const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length && lines[0].trim() !== '') {
  let idx = 0;
  const t = parseInt(lines[idx++].trim(), 10);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = parseInt(lines[idx++].trim(), 10);
    const nums: number[] = [];
    for (let i = 0; i < n; i++) nums.push(parseInt(lines[idx++].trim(), 10));
    out.push(String(firstMissingPositive(nums)));
  }
  process.stdout.write(out.join('\n'));
}
