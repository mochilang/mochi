const people = [
  {name: "Alice", city: "Paris"},
  {name: "Bob", city: "Hanoi"},
  {name: "Charlie", city: "Paris"},
  {name: "Diana", city: "Hanoi"},
  {name: "Eve", city: "Paris"},
  {name: "Frank", city: "Hanoi"},
  {name: "George", city: "Paris"}
];
const big = (() => {
  const _tmp1: Array<{ city: any; num: any }> = [];
  const groups = {};
  for (const p of people) {
    const _k = JSON.stringify(p.city);
    let g = groups[_k];
    if (!g) { g = []; g.key = p.city; g.items = g; groups[_k] = g; }
    g.push(p);
  }
  for (const _k in groups) {
    const g = groups[_k];
    if (!((g.length >= 4))) continue;
    res.push({city: g.key, num: g.length});
  }
  return res;
})()
;
console.log(JSON.stringify(big));
