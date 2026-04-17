import * as fs from "fs";

function addLists(a: number[], b: number[]): number[] {
  const out: number[] = [];
  let i = 0, j = 0, carry = 0;
  while (i < a.length || j < b.length || carry > 0) {
    let sum = carry;
    if (i < a.length) sum += a[i++];
    if (j < b.length) sum += b[j++];
    out.push(sum % 10);
    carry = Math.floor(sum / 10);
  }
  return out;
}

const fmt = (a: number[]) => `[${a.join(",")}]`;
const tokens = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (tokens.length > 0) {
  let idx = 0;
  const t = Number(tokens[idx++]);
  const lines: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(tokens[idx++]);
    const a: number[] = [];
    for (let i = 0; i < n; i++) a.push(Number(tokens[idx++]));
    const m = Number(tokens[idx++]);
    const b: number[] = [];
    for (let i = 0; i < m; i++) b.push(Number(tokens[idx++]));
    lines.push(fmt(addLists(a, b)));
  }
  process.stdout.write(lines.join("\n"));
}
