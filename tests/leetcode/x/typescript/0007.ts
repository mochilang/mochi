import * as fs from 'fs';

function reverseInt(x: number): number {
  let ans = 0;
  while (x !== 0) {
    const digit = x % 10;
    x = x < 0 ? Math.ceil(x / 10) : Math.floor(x / 10);
    if (ans > 214748364 || (ans === 214748364 && digit > 7)) return 0;
    if (ans < -214748364 || (ans === -214748364 && digit < -8)) return 0;
    ans = ans * 10 + digit;
  }
  return ans;
}

const data = fs.readFileSync(0, 'utf8');
if (data.length > 0) {
  const lines = data.split(/\r?\n/);
  const t = parseInt(lines[0].trim(), 10);
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    const x = parseInt((lines[i + 1] ?? '0').trim(), 10);
    out.push(String(reverseInt(x)));
  }
  process.stdout.write(out.join('\n'));
}
