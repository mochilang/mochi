const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
const orders = [{id: 100, customerId: 1}, {id: 101, customerId: 1}, {id: 102, customerId: 2}];
const stats = (() => {
  const groups = {};
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      const _k = JSON.stringify(c.name);
      let g = groups[_k];
      if (!g) { g = []; g.key = c.name; g.items = g; groups[_k] = g; }
      g.push(o);
    }
  }
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push({name: g.key, count: g.length});
  }
  return res;
})()
;
console.log("--- Orders per customer ---");
const _tmp2 = stats;
for (const s of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log(s.name, "orders:", s.count);
}
