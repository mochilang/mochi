const customers = [
  {id: 1, name: "Alice"},
  {id: 2, name: "Bob"},
  {id: 3, name: "Charlie"}
];
const orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 1},
  {id: 102, customerId: 2}
];
const stats = (() => {
  const _tmp1: Array<{ count: any; name: any }> = [];
  const groups = {};
  for (const c of customers) {
    for (const o of orders) {
      if (!((o.customerId == c.id))) continue;
      const _k = JSON.stringify(c.name);
      let g = groups[_k];
      if (!g) { g = []; g.key = c.name; g.items = g; groups[_k] = g; }
      g.push(c);
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    res.push({
  name: g.key,
  count: g.filter((r) => r.o).length
});
  }
  return res;
})()
;
console.log("--- Group Left Join ---");
for (const s of stats) {
  console.log(s.name, "orders:", s.count);
}
