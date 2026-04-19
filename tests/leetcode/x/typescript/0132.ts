import * as fs from 'fs';

function minCut(s: string): number {
  const n = s.length;
  const pal = Array.from({ length: n }, () => Array<boolean>(n).fill(false));
  const cuts = Array<number>(n).fill(0);
  for (let end = 0; end < n; end++) {
    cuts[end] = end;
    for (let start = 0; start <= end; start++) {
      if (s[start] === s[end] && (end - start <= 2 || pal[start + 1][end - 1])) {
        pal[start][end] = true;
        cuts[end] = start === 0 ? 0 : Math.min(cuts[end], cuts[start - 1] + 1);
      }
    }
  }
  return cuts[n - 1];
}

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0] !== '') {
  const tc = parseInt(lines[0], 10);
  const out: string[] = [];
  for (let i = 1; i <= tc; i++) out.push(String(minCut(lines[i])));
  process.stdout.write(out.join('\n\n'));
}
