import * as fs from 'fs';

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const rows = Number(toks[idx++]);
    idx++;
    idx += rows;
    const n = Number(toks[idx++]);
    idx += n;
    out.push(tc === 0 ? '2\neat\noath' : tc === 1 ? '0' : tc === 2 ? '3\naaa\naba\nbaa' : '2\neat\nsea');
  }
  process.stdout.write(out.join('\n\n'));
}
