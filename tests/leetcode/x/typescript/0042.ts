import * as fs from 'fs';

function trap(height: number[]): number {
  let left = 0;
  let right = height.length - 1;
  let leftMax = 0;
  let rightMax = 0;
  let water = 0;
  while (left <= right) {
    if (leftMax <= rightMax) {
      if (height[left] < leftMax) water += leftMax - height[left];
      else leftMax = height[left];
      left += 1;
    } else {
      if (height[right] < rightMax) water += rightMax - height[right];
      else rightMax = height[right];
      right -= 1;
    }
  }
  return water;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(lines[idx++].trim());
    const arr: number[] = [];
    for (let i = 0; i < n; i++) arr.push(Number(lines[idx++].trim()));
    out.push(String(trap(arr)));
  }
  process.stdout.write(out.join('\n'));
}
