import * as fs from "fs";

const pairs: [string, string][] = [["0", "0"], ["1", "1"], ["6", "9"], ["8", "8"], ["9", "6"]];

function build(n: number, m: number): string[] {
  if (n === 0) return [""];
  if (n === 1) return ["0", "1", "8"];
  const mids = build(n - 2, m);
  const res: string[] = [];
  for (const mid of mids) {
    for (const [a, b] of pairs) {
      if (n === m && a === "0") continue;
      res.push(a + mid + b);
    }
  }
  return res;
}

function countRange(low: string, high: string): number {
  let ans = 0;
  for (let len = low.length; len <= high.length; len++) {
    for (const s of build(len, len)) {
      if (len === low.length && s < low) continue;
      if (len === high.length && s > high) continue;
      ans++;
    }
  }
  return ans;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  let idx = 1;
  for (let i = 0; i < t; i++) {
    out.push(String(countRange((lines[idx++] ?? "").trim(), (lines[idx++] ?? "").trim())));
  }
  process.stdout.write(out.join("\n"));
}
