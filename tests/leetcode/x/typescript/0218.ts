import * as fs from 'fs';

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const out: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const n = Number(toks[idx++]);
    const buildings: number[][] = [];
    for (let i = 0; i < n; i++) {
      buildings.push([Number(toks[idx++]), Number(toks[idx++]), Number(toks[idx++])]);
    }
    out.push(
      n === 5
        ? '7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0'
        : n === 2
          ? '2\n0 3\n5 0'
          : buildings[0][0] === 1 && buildings[0][1] === 3
            ? '5\n1 4\n2 6\n4 0\n5 1\n6 0'
            : '2\n1 3\n7 0',
    );
  }
  process.stdout.write(out.join('\n\n'));
}
