import * as fs from 'fs';

function isScramble(s1: string, s2: string): boolean {
  const memo: Record<string, boolean> = {};
  function dfs(i1: number, i2: number, len: number): boolean {
    const key = `${i1},${i2},${len}`;
    if (key in memo) return memo[key];
    const a = s1.slice(i1, i1 + len), b = s2.slice(i2, i2 + len);
    if (a === b) return memo[key] = true;
    const cnt = Array(26).fill(0);
    for (let i = 0; i < len; i++) {
      cnt[a.charCodeAt(i) - 97]++;
      cnt[b.charCodeAt(i) - 97]--;
    }
    if (cnt.some((v: number) => v !== 0)) return memo[key] = false;
    for (let k = 1; k < len; k++) {
      if (dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k) ||
          dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k)) return memo[key] = true;
    }
    return memo[key] = false;
  }
  return dfs(0, 0, s1.length);
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(isScramble(lines[1 + 2 * i], lines[2 + 2 * i]) ? 'true' : 'false');
  process.stdout.write(out.join('\n'));
}
