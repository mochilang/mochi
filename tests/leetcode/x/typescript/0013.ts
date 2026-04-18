import * as fs from "fs";

const values: Record<string, number> = { I: 1, V: 5, X: 10, L: 50, C: 100, D: 500, M: 1000 };

function romanToInt(s: string): number {
  let total = 0;
  for (let i = 0; i < s.length; i++) {
    const cur = values[s[i]];
    const next = i + 1 < s.length ? values[s[i + 1]] : 0;
    total += cur < next ? -cur : cur;
  }
  return total;
}

const tokens = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (tokens.length > 0) {
  const t = Number(tokens[0]);
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(String(romanToInt(tokens[i + 1])));
  process.stdout.write(out.join("\n"));
}
