import * as fs from "fs";

function twoSum(nums: number[], target: number): [number, number] {
  for (let i = 0; i < nums.length; i++) {
    for (let j = i + 1; j < nums.length; j++) {
      if (nums[i] + nums[j] === target) {
        return [i, j];
      }
    }
  }
  return [0, 0];
}

const data = fs.readFileSync(0, "utf8").trim();
if (data.length > 0) {
  const tokens = data.split(/\s+/).map(Number);
  let idx = 0;
  const t = tokens[idx++];
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = tokens[idx++];
    const target = tokens[idx++];
    const nums = tokens.slice(idx, idx + n);
    idx += n;
    const [a, b] = twoSum(nums, target);
    out.push(`${a} ${b}`);
  }
  process.stdout.write(out.join("\n"));
}
