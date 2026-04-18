import * as fs from 'fs';
const lines = fs.readFileSync(0, 'utf8').split(/\r?\n/);
if (lines.length > 0 && lines[0].trim() !== '') {
  let idx = 0;
  const t = Number(lines[idx++].trim());
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = idx < lines.length ? Number(lines[idx++].trim()) : 0;
    const arr: number[] = [];
    for (let i = 0; i < n; i++) arr.push(idx < lines.length ? Number(lines[idx++].trim()) : 0);
    const k = idx < lines.length ? Number(lines[idx++].trim()) : 1;
    for (let i = 0; i + k <= arr.length; i += k) arr.splice(i, k, ...arr.slice(i, i + k).reverse());
    out.push('[' + arr.join(',') + ']');
  }
  process.stdout.write(out.join('\n'));
}
