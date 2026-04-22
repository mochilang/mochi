import * as fs from "fs";

function countDigitOne(n: bigint): bigint {
  let total = 0n;
  for (let m = 1n; m <= n; m *= 10n) {
    const high = n / (m * 10n);
    const cur = (n / m) % 10n;
    const low = n % m;
    if (cur === 0n) total += high * m;
    else if (cur === 1n) total += high * m + low + 1n;
    else total += (high + 1n) * m;
  }
  return total;
}

const lines = fs.readFileSync(0, "utf8").split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== "") {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    out.push(countDigitOne(BigInt((lines[i + 1] ?? "0").trim())).toString());
  }
  process.stdout.write(out.join("\n"));
}
