import * as fs from 'fs';

function maxProfit(prices: number[]): number {
  let best = 0;
  for (let i = 1; i < prices.length; i++) {
    if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
  }
  return best;
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const t = parseInt(lines[0].trim(), 10);
  let idx = 1;
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = parseInt(lines[idx++].trim(), 10);
    const prices: number[] = [];
    for (let i = 0; i < n; i++) prices.push(parseInt(lines[idx++].trim(), 10));
    out.push(String(maxProfit(prices)));
  }
  process.stdout.write(out.join('\n'));
}
