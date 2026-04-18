import * as fs from 'fs';

function myAtoi(s: string): number {
  let i = 0;
  while (i < s.length && s[i] === ' ') i++;
  let sign = 1;
  if (i < s.length && (s[i] === '+' || s[i] === '-')) {
    if (s[i] === '-') sign = -1;
    i++;
  }
  let ans = 0;
  const limit = sign > 0 ? 7 : 8;
  while (i < s.length && s[i] >= '0' && s[i] <= '9') {
    const digit = s.charCodeAt(i) - '0'.charCodeAt(0);
    if (ans > 214748364 || (ans === 214748364 && digit > limit)) return sign > 0 ? 2147483647 : -2147483648;
    ans = ans * 10 + digit;
    i++;
  }
  return sign * ans;
}

const data = fs.readFileSync(0, 'utf8');
if (data.length > 0) {
  const lines = data.split(/\r?\n/);
  const t = parseInt(lines[0].trim(), 10);
  const out: string[] = [];
  for (let i = 0; i < t; i++) out.push(String(myAtoi(lines[i + 1] ?? '')));
  process.stdout.write(out.join('\n'));
}
