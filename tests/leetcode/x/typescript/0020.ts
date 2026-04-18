import * as fs from "fs";

function isValid(s: string): boolean {
  const stack: string[] = [];
  for (const ch of s) {
    if (ch === '(' || ch === '[' || ch === '{') {
      stack.push(ch);
    } else {
      if (stack.length === 0) return false;
      const open = stack.pop()!;
      if ((ch === ')' && open !== '(') ||
          (ch === ']' && open !== '[') ||
          (ch === '}' && open !== '{')) return false;
    }
  }
  return stack.length === 0;
}

const tokens = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (tokens.length > 0) {
  const t = Number(tokens[0]);
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(isValid(tokens[i + 1]) ? "true" : "false");
  process.stdout.write(out.join("\n"));
}
