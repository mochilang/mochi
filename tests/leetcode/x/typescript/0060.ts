import * as fs from 'fs';

function getPermutation(n: number, kInput: number): string {
  const digits: string[] = [];
  for (let i = 1; i <= n; i++) digits.push(String(i));
  const fact = Array(n + 1).fill(1);
  for (let i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
  let k = kInput - 1;
  let out = '';
  for (let rem = n; rem >= 1; rem--) {
    const block = fact[rem - 1];
    const idx = Math.floor(k / block);
    k %= block;
    out += digits.splice(idx, 1)[0];
  }
  return out;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(lines[idx++].trim());
    const k = Number(lines[idx++].trim());
    out.push(getPermutation(n, k));
  }
  process.stdout.write(out.join('\n'));
}
