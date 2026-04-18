import * as fs from "fs";

function longest(s: string): number {
  const last = new Map<string, number>();
  let left = 0;
  let best = 0;
  for (let right = 0; right < s.length; right++) {
    const ch = s[right];
    if (last.has(ch) && last.get(ch)! >= left) left = last.get(ch)! + 1;
    last.set(ch, right);
    best = Math.max(best, right - left + 1);
  }
  return best;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(String(longest(lines[i + 1] ?? "")));
  process.stdout.write(out.join("\n"));
}
