const items = [
  { cat: "a", val: 3 },
  { cat: "a", val: 1 },
  { cat: "b", val: 5 },
  { cat: "b", val: 2 },
];
const groups: Record<string, number> = {};
for (const i of items) {
  groups[i.cat] = (groups[i.cat] || 0) + i.val;
}
const result = Object.entries(groups)
  .sort((a, b) => b[1] - a[1])
  .map(([cat, total]) => ({ cat, total }));
console.log(result);
