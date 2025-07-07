const data = [
  { tag: "a", val: 1 },
  { tag: "a", val: 2 },
  { tag: "b", val: 3 },
];
const groups: Record<string, Array<{ tag: string; val: number }>> = {};
for (const d of data) {
  if (!groups[d.tag]) groups[d.tag] = [];
  groups[d.tag].push(d);
}
const tmp: Array<{ tag: string; total: number }> = [];
for (const [tag, items] of Object.entries(groups)) {
  let total = 0;
  for (const x of items) {
    total += x.val;
  }
  tmp.push({ tag, total });
}
const result = tmp.sort((a, b) => (a.tag < b.tag ? -1 : a.tag > b.tag ? 1 : 0));
console.log(result);
