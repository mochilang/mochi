import * as fs from 'fs';

function convertZigzag(s: string, numRows: number): string {
  if (numRows <= 1 || numRows >= s.length) return s;
  const cycle = 2 * numRows - 2;
  let out = '';
  for (let row = 0; row < numRows; row++) {
    for (let i = row; i < s.length; i += cycle) {
      out += s[i];
      const diag = i + cycle - 2 * row;
      if (row > 0 && row < numRows - 1 && diag < s.length) out += s[diag];
    }
  }
  return out;
}

const data = fs.readFileSync(0, 'utf8');
if (data.length > 0) {
  const lines = data.split(/\r?\n/);
  const t = parseInt(lines[0].trim(), 10);
  const out: string[] = [];
  let idx = 1;
  for (let i = 0; i < t; i++) {
    const s = lines[idx] ?? '';
    idx += 1;
    const numRows = parseInt((lines[idx] ?? '1').trim(), 10);
    idx += 1;
    out.push(convertZigzag(s, numRows));
  }
  process.stdout.write(out.join('\n'));
}
