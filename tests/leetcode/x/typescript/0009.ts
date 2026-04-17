import * as fs from "fs";

function isPalindrome(x: number): boolean {
  if (x < 0) return false;
  const original = x;
  let n = x;
  let rev = 0;
  while (n > 0) {
    rev = rev * 10 + (n % 10);
    n = Math.floor(n / 10);
  }
  return rev === original;
}

const tokens = fs.readFileSync(0, "utf8").trim().split(/\s+/).filter(Boolean);
if (tokens.length > 0) {
  const t = Number(tokens[0]);
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    out.push(isPalindrome(Number(tokens[i + 1])) ? "true" : "false");
  }
  process.stdout.write(out.join("\n"));
}
