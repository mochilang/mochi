const items = [
  { cat: "a", val: 10, flag: true },
  { cat: "a", val: 5, flag: false },
  { cat: "b", val: 20, flag: true },
];
interface Result { cat: string; share: number; }
const groups: Record<string, { sumVal: number; sumAll: number }> = {};
for (const i of items) {
  const g = groups[i.cat] || { sumVal: 0, sumAll: 0 };
  g.sumAll += i.val;
  if (i.flag) g.sumVal += i.val;
  groups[i.cat] = g;
}
const result: Result[] = Object.entries(groups)
  .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
  .map(([cat, g]) => ({ cat, share: g.sumVal / g.sumAll }));
console.log(result);
