import * as fs from 'fs';

type Row = { dept: string; name: string; salary: number };

const toks = fs.readFileSync(0, 'utf8').trim().split(/\s+/).filter((x: string) => x.length > 0);
if (toks.length > 0) {
  let idx = 0;
  const t = Number(toks[idx++]);
  const cases: string[] = [];
  for (let tc = 0; tc < t; tc++) {
    const d = Number(toks[idx++]);
    const e = Number(toks[idx++]);
    const deptName = new Map<number, string>();
    for (let i = 0; i < d; i++) deptName.set(Number(toks[idx++]), toks[idx++]);
    const groups = new Map<number, Array<[string, number]>>();
    for (let i = 0; i < e; i++) {
      idx++;
      const name = toks[idx++];
      const salary = Number(toks[idx++]);
      const deptId = Number(toks[idx++]);
      if (!groups.has(deptId)) groups.set(deptId, []);
      groups.get(deptId)!.push([name, salary]);
    }
    const rows: Row[] = [];
    for (const [deptId, items] of groups) {
      const keep = [...new Set(items.map((x) => x[1]))].sort((a, b) => b - a).slice(0, 3);
      for (const [name, salary] of items) if (keep.includes(salary)) rows.push({ dept: deptName.get(deptId)!, name, salary });
    }
    rows.sort((a, b) => a.dept.localeCompare(b.dept) || b.salary - a.salary || a.name.localeCompare(b.name));
    cases.push([String(rows.length), ...rows.map((r) => `${r.dept},${r.name},${r.salary}`)].join('\n'));
  }
  process.stdout.write(cases.join('\n\n'));
}
