function _json(v: any) {
  function _sort(x: any): any {
    if (Array.isArray(x)) return x.map(_sort);
    if (x && typeof x === "object") {
      const keys = Object.keys(x).sort();
      const o: any = {};
      for (const k of keys) o[k] = _sort(x[k]);
      return o;
    }
    return x;
  }
  return JSON.stringify(_sort(v));
}
const supplier = [
  {
  s_suppkey: 100,
  s_name: "Best Supplier",
  s_address: "123 Market St",
  s_phone: "123-456"
},
  {
  s_suppkey: 200,
  s_name: "Second Supplier",
  s_address: "456 Elm St",
  s_phone: "987-654"
}
];
const lineitem = [
  {
  l_suppkey: 100,
  l_extendedprice: 1000,
  l_discount: 0.1,
  l_shipdate: "1996-01-15"
},
  {
  l_suppkey: 100,
  l_extendedprice: 500,
  l_discount: 0,
  l_shipdate: "1996-03-20"
},
  {
  l_suppkey: 200,
  l_extendedprice: 800,
  l_discount: 0.05,
  l_shipdate: "1995-12-30"
}
];
const start_date = "1996-01-01";
const end_date = "1996-04-01";
const revenue0 = (() => {
  const _tmp1: Array<{ supplier_no: any; total_revenue: number }> = [];
  const groups = {};
  for (const l of lineitem) {
    if (!(((l.l_shipdate >= start_date) && (l.l_shipdate < end_date)))) continue;
    const _k = JSON.stringify(l.l_suppkey);
    let g = groups[_k];
    if (!g) { g = []; g.key = l.l_suppkey; g.items = g; groups[_k] = g; }
    g.push(l);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({
  supplier_no: g.key,
  total_revenue: (g.map((x) => (x.l_extendedprice * ((1 - x.l_discount)))).reduce((a,b)=>a+b,0))
});
  }
  return _tmp1;
})()
;
const revenues = revenue0.map((x) => x.total_revenue);
const max_revenue = Math.max(...revenues);
const result = (() => {
  let _tmp2: Array<{ s_address: any; s_name: any; s_phone: any; s_suppkey: any; total_revenue: any }> = [];
  for (const s of supplier) {
    for (const r of revenue0) {
      if (!((s.s_suppkey == r.supplier_no))) continue;
      if (!((r.total_revenue == max_revenue))) continue;
      _tmp2.push({item: {
  s_suppkey: s.s_suppkey,
  s_name: s.s_name,
  s_address: s.s_address,
  s_phone: s.s_phone,
  total_revenue: r.total_revenue
}, key: s.s_suppkey});
    }
  }
  _tmp2 = _tmp2.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp2;
})()
;
console.log(_json(result));
const rev = ((1000 * 0.9) + 500);
if (!(JSON.stringify(result) === JSON.stringify([
  {
  s_suppkey: 100,
  s_name: "Best Supplier",
  s_address: "123 Market St",
  s_phone: "123-456",
  total_revenue: rev
}
]))) { throw new Error("Q15 returns top revenue supplier(s) for Q1 1996 failed"); }
