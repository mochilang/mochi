import * as fs from 'fs';

function expand(s: string, left: number, right: number): [number, number] {
  while (left >= 0 && right < s.length && s[left] === s[right]) {
    left--;
    right++;
  }
  return [left + 1, right - left - 1];
}

function longestPalindrome(s: string): string {
  let bestStart = 0;
  let bestLen = s.length > 0 ? 1 : 0;
  for (let i = 0; i < s.length; i++) {
    const odd = expand(s, i, i);
    if (odd[1] > bestLen) {
      bestStart = odd[0];
      bestLen = odd[1];
    }
    const even = expand(s, i, i + 1);
    if (even[1] > bestLen) {
      bestStart = even[0];
      bestLen = even[1];
    }
  }
  return s.slice(bestStart, bestStart + bestLen);
}

const data = fs.readFileSync(0, 'utf8');
if (data.length > 0) {
  const lines = data.split(/\r?\n/);
  const t = parseInt(lines[0].trim(), 10);
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    out.push(longestPalindrome(lines[i + 1] ?? ''));
  }
  process.stdout.write(out.join('\n'));
}
