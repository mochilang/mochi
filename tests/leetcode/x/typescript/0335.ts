import * as fs from "fs";
function isSelfCrossing(x: number[]): boolean { for (let i = 3; i < x.length; i++) { if (x[i] >= x[i - 2] && x[i - 1] <= x[i - 3]) return true; if (i >= 4 && x[i - 1] === x[i - 3] && x[i] + x[i - 4] >= x[i - 2]) return true; if (i >= 5 && x[i - 2] >= x[i - 4] && x[i] + x[i - 4] >= x[i - 2] && x[i - 1] <= x[i - 3] && x[i - 1] + x[i - 5] >= x[i - 3]) return true; } return false; }
const data = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean).map(Number);
if (data.length > 0) { let idx = 0; const t = data[idx++]; const out: string[] = []; for (let tc = 0; tc < t; tc++) { const n = data[idx++]; const x = data.slice(idx, idx + n); idx += n; out.push(isSelfCrossing(x) ? "true" : "false"); } process.stdout.write(out.join("\n\n")); }
