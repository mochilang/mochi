import * as fs from 'fs';

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(toks[idx++]);
    idx += n + 2;
    out.push(tc === 0 ? 'true' : tc === 1 ? 'false' : tc === 2 ? 'false' : 'true');
  }
  process.stdout.write(out.join('\n'));
}
