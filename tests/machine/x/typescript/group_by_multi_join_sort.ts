const nation = [{n_nationkey: 1, n_name: "BRAZIL"}];
const customer = [
  {
  c_custkey: 1,
  c_name: "Alice",
  c_acctbal: 100,
  c_nationkey: 1,
  c_address: "123 St",
  c_phone: "123-456",
  c_comment: "Loyal"
}
];
const orders = [
  {
  o_orderkey: 1000,
  o_custkey: 1,
  o_orderdate: "1993-10-15"
},
  {
  o_orderkey: 2000,
  o_custkey: 1,
  o_orderdate: "1994-01-02"
}
];
const lineitem = [
  {
  l_orderkey: 1000,
  l_returnflag: "R",
  l_extendedprice: 1000,
  l_discount: 0.1
},
  {
  l_orderkey: 2000,
  l_returnflag: "N",
  l_extendedprice: 500,
  l_discount: 0
}
];
const start_date = "1993-10-01";
const end_date = "1994-01-01";
const result = (() => {
  const _tmp1: Array<{ c_acctbal: any; c_address: any; c_comment: any; c_custkey: any; c_name: any; c_phone: any; n_name: any; revenue: any }> = [];
  const groups = {};
  for (const c of customer) {
    for (const o of orders) {
      if (!((o.o_custkey == c.c_custkey))) continue;
      for (const l of lineitem) {
        if (!((l.l_orderkey == o.o_orderkey))) continue;
        for (const n of nation) {
          if (!((n.n_nationkey == c.c_nationkey))) continue;
          if (!((((((o.o_orderdate >= start_date) && o.o_orderdate) < end_date) && l.l_returnflag) == "R"))) continue;
          const _k = JSON.stringify({
  c_custkey: c.c_custkey,
  c_name: c.c_name,
  c_acctbal: c.c_acctbal,
  c_address: c.c_address,
  c_phone: c.c_phone,
  c_comment: c.c_comment,
  n_name: n.n_name
});
          let g = groups[_k];
          if (!g) { g = []; g.key = {
  c_custkey: c.c_custkey,
  c_name: c.c_name,
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
    res.push({item: {
  c_custkey: g.key.c_custkey,
  c_name: g.key.c_name,
  revenue: (g.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0)),
  c_acctbal: g.key.c_acctbal,
  n_name: g.key.n_name,
  c_address: g.key.c_address,
  c_phone: g.key.c_phone,
  c_comment: g.key.c_comment
}, key: (-(g.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0)))});
  }
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(result);
