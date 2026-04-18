import * as fs from 'fs';
const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const k = idx < lines.length ? Number(lines[idx++].trim()) : 0;
    const vals: number[] = [];
    for (let i = 0; i < k; i++) {
      const n = idx < lines.length ? Number(lines[idx++].trim()) : 0;
      for (let j = 0; j < n; j++) vals.push(idx < lines.length ? Number(lines[idx++].trim()) : 0);
    }
    vals.sort((a, b) => a - b);
    out.push('[' + vals.join(',') + ']');
  }
  process.stdout.write(out.join('\n'));
}
