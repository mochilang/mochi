import * as fs from 'fs';

function solve(k: number, prices: number[]): number {
  const n = prices.length;
  if (k >= Math.floor(n / 2)) {
    let best = 0;
    for (let i = 1; i < n; i++) if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
    return best;
  }
  const negInf = -Number.MAX_SAFE_INTEGER;
  const buy = Array<number>(k + 1).fill(negInf);
  const sell = Array<number>(k + 1).fill(0);
  for (const price of prices) {
    for (let t = 1; t <= k; t++) {
      buy[t] = Math.max(buy[t], sell[t - 1] - price);
      sell[t] = Math.max(sell[t], buy[t] + price);
    }
  }
  return sell[k];
}

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const k = Number(toks[idx++]);
    const n = Number(toks[idx++]);
    const prices: number[] = [];
    for (let i = 0; i < n; i++) prices.push(Number(toks[idx++]));
    out.push(String(solve(k, prices)));
  }
  process.stdout.write(out.join('\n'));
}
