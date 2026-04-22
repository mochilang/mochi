import * as fs from "fs";

function solve(words: string[]): string {
  const chars = new Set(words.join("").split(""));
  const adj = new Map<string, Set<string>>();
  const indeg = new Map<string, number>();
  for (const c of chars) {
    adj.set(c, new Set());
    indeg.set(c, 0);
  }
  for (let i = 0; i + 1 < words.length; i++) {
    const a = words[i], b = words[i + 1];
    const m = Math.min(a.length, b.length);
    if (a.slice(0, m) === b.slice(0, m) && a.length > b.length) return "";
    for (let j = 0; j < m; j++) {
      if (a[j] !== b[j]) {
        if (!adj.get(a[j])!.has(b[j])) {
          adj.get(a[j])!.add(b[j]);
          indeg.set(b[j], indeg.get(b[j])! + 1);
        }
        break;
      }
    }
  }
  const zeros = Array.from(chars).filter(c => indeg.get(c) === 0).sort();
  const out: string[] = [];
  while (zeros.length > 0) {
    const c = zeros.shift()!;
    out.push(c);
    const nexts = Array.from(adj.get(c)!).sort();
    for (const nei of nexts) {
      indeg.set(nei, indeg.get(nei)! - 1);
      if (indeg.get(nei) === 0) {
        zeros.push(nei);
        zeros.sort();
      }
    }
  }
  return out.length === chars.size ? out.join("") : "";
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  let idx = 1;
  for (let tc = 0; tc < t; tc++) {
    const n = Number(lines[idx++].trim());
    out.push(solve(lines.slice(idx, idx + n).map(s => s.trim())));
    idx += n;
  }
  process.stdout.write(out.join("\n"));
}
