import * as fs from 'fs';

function maxProfit(prices: number[]): number {
  let buy1 = -1_000_000_000, sell1 = 0, buy2 = -1_000_000_000, sell2 = 0;
  for (const p of prices) {
    buy1 = Math.max(buy1, -p);
    sell1 = Math.max(sell1, buy1 + p);
    buy2 = Math.max(buy2, sell1 - p);
    sell2 = Math.max(sell2, buy2 + p);
  }
  return sell2;
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
