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
const nation = [{n_nationkey: 1, n_name: "GERMANY"}];
const customer = [
  {
  c_custkey: 1,
  c_name: "Alice",
  c_acctbal: 1000,
  c_nationkey: 1,
  c_address: "123 Market St",
  c_phone: "123-456",
  c_comment: "Premium client"
},
  {
  c_custkey: 2,
  c_name: "Bob",
  c_acctbal: 200,
  c_nationkey: 1,
  c_address: "456 Side St",
  c_phone: "987-654",
  c_comment: "Frequent returns"
}
];
const orders = [
  {o_orderkey: 100, o_custkey: 1},
  {o_orderkey: 200, o_custkey: 1},
  {o_orderkey: 300, o_custkey: 2}
];
const lineitem = [
  {
  l_orderkey: 100,
  l_quantity: 150,
  l_extendedprice: 1000,
  l_discount: 0.1
},
  {
  l_orderkey: 200,
  l_quantity: 100,
  l_extendedprice: 800,
  l_discount: 0
},
  {
  l_orderkey: 300,
  l_quantity: 30,
  l_extendedprice: 300,
  l_discount: 0.05
}
];
const threshold = 200;
const result = (() => {
  let _tmp1: Array<{ c_acctbal: any; c_address: any; c_comment: any; c_custkey: any; c_name: any; c_phone: any; n_name: any; revenue: number }> = [];
  const groups = {};
  for (const c of customer) {
    for (const o of orders) {
      if (!((o.o_custkey == c.c_custkey))) continue;
      for (const l of lineitem) {
        if (!((l.l_orderkey == o.o_orderkey))) continue;
        for (const n of nation) {
          if (!((n.n_nationkey == c.c_nationkey))) continue;
          const _k = JSON.stringify({
  c_name: c.c_name,
  c_custkey: c.c_custkey,
  c_acctbal: c.c_acctbal,
  c_address: c.c_address,
  c_phone: c.c_phone,
  c_comment: c.c_comment,
  n_name: n.n_name
});
          let g = groups[_k];
          if (!g) { g = []; g.key = {
  c_name: c.c_name,
  c_custkey: c.c_custkey,
  c_acctbal: c.c_acctbal,
  c_address: c.c_address,
  c_phone: c.c_phone,
  c_comment: c.c_comment,
  n_name: n.n_name
}; g.items = g; groups[_k] = g; }
          g.push({c: c, o: o, l: l, n: n});
        }
      }
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    if (!(((g.map((x) => x.l.l_quantity).reduce((a,b)=>a+b,0)) > threshold))) continue;
    _tmp1.push({item: {
  c_name: g.key.c_name,
  c_custkey: g.key.c_custkey,
  revenue: (g.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0)),
  c_acctbal: g.key.c_acctbal,
  n_name: g.key.n_name,
  c_address: g.key.c_address,
  c_phone: g.key.c_phone,
  c_comment: g.key.c_comment
}, key: (-(g.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0)))});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  c_name: "Alice",
  c_custkey: 1,
  revenue: 1700,
  c_acctbal: 1000,
  n_name: "GERMANY",
  c_address: "123 Market St",
  c_phone: "123-456",
  c_comment: "Premium client"
}
]))) { throw new Error("Q18 returns large-volume customers with total quantity > 200 failed"); }
