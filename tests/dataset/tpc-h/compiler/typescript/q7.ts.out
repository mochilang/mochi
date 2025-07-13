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
const nation = [
  {n_nationkey: 1, n_name: "FRANCE"},
  {n_nationkey: 2, n_name: "GERMANY"}
];
const supplier = [{s_suppkey: 100, s_nationkey: 1}];
const customer = [{c_custkey: 200, c_nationkey: 2}];
const orders = [{o_orderkey: 1000, o_custkey: 200}];
const lineitem = [
  {
  l_orderkey: 1000,
  l_suppkey: 100,
  l_extendedprice: 1000,
  l_discount: 0.1,
  l_shipdate: "1995-06-15"
},
  {
  l_orderkey: 1000,
  l_suppkey: 100,
  l_extendedprice: 800,
  l_discount: 0.05,
  l_shipdate: "1997-01-01"
}
];
const start_date = "1995-01-01";
const end_date = "1996-12-31";
const nation1 = "FRANCE";
const nation2 = "GERMANY";
const result = (() => {
  let _tmp1: Array<{ cust_nation: any; l_year: any; revenue: number; supp_nation: any }> = [];
  const groups = {};
  for (const l of lineitem) {
    for (const o of orders) {
      if (!((o.o_orderkey == l.l_orderkey))) continue;
      for (const c of customer) {
        if (!((c.c_custkey == o.o_custkey))) continue;
        for (const s of supplier) {
          if (!((s.s_suppkey == l.l_suppkey))) continue;
          for (const n1 of nation) {
            if (!((n1.n_nationkey == s.s_nationkey))) continue;
            for (const n2 of nation) {
              if (!((n2.n_nationkey == c.c_nationkey))) continue;
              if (!((((((l.l_shipdate >= start_date) && (l.l_shipdate <= end_date)) && (((n1.n_name == nation1) && (n2.n_name == nation2)))) || (((n1.n_name == nation2) && (n2.n_name == nation1))))))) continue;
              const _k = JSON.stringify({
  supp_nation: n1.n_name,
  cust_nation: n2.n_name,
  l_year: l.l_shipdate.substring(0, 4)
});
              let g = groups[_k];
              if (!g) { g = []; g.key = {
  supp_nation: n1.n_name,
  cust_nation: n2.n_name,
  l_year: l.l_shipdate.substring(0, 4)
}; g.items = g; groups[_k] = g; }
              g.push({l: l, o: o, c: c, s: s, n1: n1, n2: n2});
            }
          }
        }
      }
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {
  supp_nation: g.key.supp_nation,
  cust_nation: g.key.cust_nation,
  l_year: g.key.l_year,
  revenue: (g.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0))
}, key: [
  g.key.supp_nation,
  g.key.cust_nation,
  g.key.l_year
]});
  }
  _tmp1 = _tmp1.sort((a,b)=> JSON.stringify(a.key) < JSON.stringify(b.key) ? -1 : JSON.stringify(a.key) > JSON.stringify(b.key) ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  supp_nation: "FRANCE",
  cust_nation: "GERMANY",
  l_year: "1995",
  revenue: 900
}
]))) { throw new Error("Q7 computes revenue between FRANCE and GERMANY by year failed"); }
