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
  {n_nationkey: 1, n_name: "BRAZIL"},
  {n_nationkey: 2, n_name: "CANADA"}
];
const supplier = [
  {s_suppkey: 100, s_nationkey: 1},
  {s_suppkey: 200, s_nationkey: 2}
];
const part = [
  {
  p_partkey: 1000,
  p_name: "green metal box"
},
  {
  p_partkey: 2000,
  p_name: "red plastic crate"
}
];
const partsupp = [
  {
  ps_partkey: 1000,
  ps_suppkey: 100,
  ps_supplycost: 10
},
  {
  ps_partkey: 1000,
  ps_suppkey: 200,
  ps_supplycost: 8
}
];
const orders = [
  {
  o_orderkey: 1,
  o_orderdate: "1995-02-10"
},
  {
  o_orderkey: 2,
  o_orderdate: "1997-01-01"
}
];
const lineitem = [
  {
  l_orderkey: 1,
  l_partkey: 1000,
  l_suppkey: 100,
  l_quantity: 5,
  l_extendedprice: 1000,
  l_discount: 0.1
},
  {
  l_orderkey: 2,
  l_partkey: 1000,
  l_suppkey: 200,
  l_quantity: 10,
  l_extendedprice: 800,
  l_discount: 0.05
}
];
const prefix = "green";
const start_date = "1995-01-01";
const end_date = "1996-12-31";
const result = (() => {
  let _tmp1: Array<{ nation: any; o_year: string; profit: number }> = [];
  const groups = {};
  for (const l of lineitem) {
    for (const p of part) {
      if (!((p.p_partkey == l.l_partkey))) continue;
      for (const s of supplier) {
        if (!((s.s_suppkey == l.l_suppkey))) continue;
        for (const ps of partsupp) {
          if (!(((ps.ps_partkey == l.l_partkey) && (ps.ps_suppkey == l.l_suppkey)))) continue;
          for (const o of orders) {
            if (!((o.o_orderkey == l.l_orderkey))) continue;
            for (const n of nation) {
              if (!((n.n_nationkey == s.s_nationkey))) continue;
              if (!((((p.p_name.substring(0, prefix.length) == prefix) && (o.o_orderdate >= start_date)) && (o.o_orderdate <= end_date)))) continue;
              const _k = JSON.stringify({
  nation: n.n_name,
  o_year: o.o_orderdate.substring(0, 4)
});
              let g = groups[_k];
              if (!g) { g = []; g.key = {
  nation: n.n_name,
  o_year: o.o_orderdate.substring(0, 4)
}; g.items = g; groups[_k] = g; }
              g.push({l: l, p: p, s: s, ps: ps, o: o, n: n});
            }
          }
        }
      }
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {
  nation: g.key.nation,
  o_year: String(g.key.o_year),
  profit: (g.map((x) => (((x.l.l_extendedprice * ((1 - x.l.l_discount)))) - ((x.ps.ps_supplycost * x.l.l_quantity)))).reduce((a,b)=>a+b,0))
}, key: [g.key.nation, (-g.key.o_year)]});
  }
  _tmp1 = _tmp1.sort((a,b)=> JSON.stringify(a.key) < JSON.stringify(b.key) ? -1 : JSON.stringify(a.key) > JSON.stringify(b.key) ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
const revenue = (1000 * 0.9);
const cost = (5 * 10);
if (!(JSON.stringify(result) === JSON.stringify([
  {
  nation: "BRAZIL",
  o_year: "1995",
  profit: (revenue - cost)
}
]))) { throw new Error("Q9 computes profit for green parts by nation and year failed"); }
