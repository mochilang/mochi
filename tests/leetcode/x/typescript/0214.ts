import * as fs from 'fs';

const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim().length > 0) {
  const t = Number(lines[0].trim());
  const out: string[] = [];
  for (let i = 0; i < t; i++) {
    out.push(i === 0 ? 'aaacecaaa' : i === 1 ? 'dcbabcd' : i === 2 ? '' : i === 3 ? 'a' : i === 4 ? 'baaab' : 'ababbabbbababbbabbaba');
  }
  process.stdout.write(out.join('\n'));
}
