let customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}];
let orders = [{id: 100, customerId: 1}, {id: 101, customerId: 1}, {id: 102, customerId: 2}];
let stats = (() => {
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
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push({name: g.key, count: (() => {
  const _tmp1 = [];
  for (const r of g) {
    if (!(r.o)) continue;
    _tmp1.push(r);
  }
  let res = _tmp1;
  return res;
})()
.length});
  }
  return res;
})()
;
console.log("--- Group Left Join ---");
const _tmp3 = stats;
for (const s of (Array.isArray(_tmp3) ? _tmp3 : Object.keys(_tmp3))) {
  console.log(s.name, "orders:", s.count);
}
