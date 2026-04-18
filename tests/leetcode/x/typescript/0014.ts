import * as fs from "fs";

function lcp(strs: string[]): string {
  let prefix = strs[0];
  while (!strs.every((s) => s.startsWith(prefix))) {
    prefix = prefix.slice(0, -1);
  }
  return prefix;
}

const tokens = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (tokens.length > 0) {
  let idx = 0;
  const t = Number(tokens[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(tokens[idx++]);
    const strs = tokens.slice(idx, idx + n);
    idx += n;
    out.push(`"${lcp(strs)}"`);
  }
  process.stdout.write(out.join("\n"));
}
