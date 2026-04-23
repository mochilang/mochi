import * as fs from "fs";
function minPatches(nums: number[], n: number): number { let miss = 1; let i = 0; let patches = 0; while (miss <= n) { if (i < nums.length && nums[i] <= miss) miss += nums[i++]; else { miss += miss; patches++; } } return patches; }
const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) { let idx = 0; const t = data[idx++]; const out: string[] = []; for (let tc = 0; tc < t; tc++) { const size = data[idx++]; const nums = data.slice(idx, idx + size); idx += size; const n = data[idx++]; out.push(String(minPatches(nums, n))); } process.stdout.write(out.join("\n\n")); }
