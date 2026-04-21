import * as fs from 'fs';

function candy(ratings: number[]): number {
  const n = ratings.length;
  const candies = Array<number>(n).fill(1);
  for (let i = 1; i < n; i++) {
    if (ratings[i] > ratings[i - 1]) candies[i] = candies[i - 1] + 1;
  }
  for (let i = n - 2; i >= 0; i--) {
    if (ratings[i] > ratings[i + 1]) candies[i] = Math.max(candies[i], candies[i + 1] + 1);
  }
  return candies.reduce((a, b) => a + b, 0);
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  let idx = 1;
  const out: string[] = [];
  for (let t = 0; t < tc; t++) {
    const n = parseInt(lines[idx++], 10);
    const ratings = lines.slice(idx, idx + n).map((s) => parseInt(s, 10));
    idx += n;
    out.push(String(candy(ratings)));
  }
  process.stdout.write(out.join('\n\n'));
}
