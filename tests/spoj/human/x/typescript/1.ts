// https://www.spoj.com/problems/TEST/
import * as fs from 'fs';

const data = fs.readFileSync(0, 'utf8').trim().split(/\s+/).map(Number);
for (const n of data) {
  if (n === 42) break;
  console.log(n);
}
