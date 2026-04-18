import * as fs from 'fs';

function maxProfit(prices: number[]): number {
  if (prices.length === 0) return 0;
  let minPrice = prices[0], best = 0;
  for (let i = 1; i < prices.length; i++) {
    best = Math.max(best, prices[i] - minPrice);
    minPrice = Math.min(minPrice, prices[i]);
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
