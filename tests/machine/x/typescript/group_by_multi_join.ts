const nations = [{id: 1, name: "A"}, {id: 2, name: "B"}];
const suppliers = [{id: 1, nation: 1}, {id: 2, nation: 2}];
const partsupp = [{part: 100, supplier: 1, cost: 10, qty: 2}, {part: 100, supplier: 2, cost: 20, qty: 1}, {part: 200, supplier: 1, cost: 5, qty: 3}];
const filtered = (() => {
  const _tmp1 = [];
  for (const ps of partsupp) {
    for (const s of suppliers) {
      if (!((s.id == ps.supplier))) continue;
      for (const n of nations) {
        if (!((n.id == s.nation))) continue;
        if (!((n.name == "A"))) continue;
        _tmp1.push({part: ps.part, value: (ps.cost * ps.qty)});
      }
    }
  }
  let res = _tmp1;
  return res;
})()
;
const grouped = (() => {
  const groups = {};
  for (const x of filtered) {
    const _k = JSON.stringify(x.part);
    let g = groups[_k];
    if (!g) { g = []; g.key = x.part; g.items = g; groups[_k] = g; }
    g.push(x);
  }
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push({part: g.key, total: ((() => {
  const _tmp2 = [];
  for (const r of g) {
    _tmp2.push(r.value);
  }
  let res = _tmp2;
  return res;
})()
.reduce((a,b)=>a+b,0))});
  }
  return res;
})()
;
console.log(grouped);
