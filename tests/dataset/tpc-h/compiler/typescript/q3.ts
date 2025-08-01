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
const customer = [
  {c_custkey: 1, c_mktsegment: "BUILDING"},
  {
  c_custkey: 2,
  c_mktsegment: "AUTOMOBILE"
}
];
const orders = [
  {
  o_orderkey: 100,
  o_custkey: 1,
  o_orderdate: "1995-03-14",
  o_shippriority: 1
},
  {
  o_orderkey: 200,
  o_custkey: 2,
  o_orderdate: "1995-03-10",
  o_shippriority: 2
}
];
const lineitem = [
  {
  l_orderkey: 100,
  l_extendedprice: 1000,
  l_discount: 0.05,
  l_shipdate: "1995-03-16"
},
  {
  l_orderkey: 100,
  l_extendedprice: 500,
  l_discount: 0,
  l_shipdate: "1995-03-20"
},
  {
  l_orderkey: 200,
  l_extendedprice: 1000,
  l_discount: 0.1,
  l_shipdate: "1995-03-14"
}
];
const cutoff = "1995-03-15";
const segment = "BUILDING";
const building_customers = customer.filter((c) => (c.c_mktsegment == segment));
const valid_orders = (() => {
  const _tmp1: any[] = [];
  for (const o of orders) {
    for (const c of building_customers) {
      if (!((o.o_custkey == c.c_custkey))) continue;
      if (!((o.o_orderdate < cutoff))) continue;
      _tmp1.push(o);
    }
  }
  return _tmp1;
})()
;
const valid_lineitems = lineitem.filter((l) => (l.l_shipdate > cutoff));
const order_line_join = (() => {
  let _tmp2: Array<{ l_orderkey: any; o_orderdate: any; o_shippriority: any; revenue: number }> = [];
  const groups = {};
  for (const o of valid_orders) {
    for (const l of valid_lineitems) {
      if (!((l.l_orderkey == o.o_orderkey))) continue;
      const _k = JSON.stringify({
  o_orderkey: o.o_orderkey,
  o_orderdate: o.o_orderdate,
  o_shippriority: o.o_shippriority
});
      let g = groups[_k];
      if (!g) { g = []; g.key = {
  o_orderkey: o.o_orderkey,
  o_orderdate: o.o_orderdate,
  o_shippriority: o.o_shippriority
}; g.items = g; groups[_k] = g; }
      g.push({o: o, l: l});
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp2.push({item: {
  l_orderkey: g.key.o_orderkey,
  revenue: (g.map((r) => (r.l.l_extendedprice * ((1 - r.l.l_discount)))).reduce((a,b)=>a+b,0)),
  o_orderdate: g.key.o_orderdate,
  o_shippriority: g.key.o_shippriority
}, key: [
  (-(g.map((r) => (r.l.l_extendedprice * ((1 - r.l.l_discount)))).reduce((a,b)=>a+b,0))),
  g.key.o_orderdate
]});
  }
  _tmp2 = _tmp2.sort((a,b)=> JSON.stringify(a.key) < JSON.stringify(b.key) ? -1 : JSON.stringify(a.key) > JSON.stringify(b.key) ? 1 : 0).map(x=>x.item);
  return _tmp2;
})()
;
console.log(_json(order_line_join));
if (!(JSON.stringify(order_line_join) === JSON.stringify([
  {
  l_orderkey: 100,
  revenue: ((1000 * 0.95) + 500),
  o_orderdate: "1995-03-14",
  o_shippriority: 1
}
]))) { throw new Error("Q3 returns revenue per order with correct priority failed"); }
