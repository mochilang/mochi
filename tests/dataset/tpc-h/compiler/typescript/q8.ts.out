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
const region = [{r_regionkey: 0, r_name: "AMERICA"}];
const nation = [
  {
  n_nationkey: 10,
  n_regionkey: 0,
  n_name: "BRAZIL"
},
  {
  n_nationkey: 20,
  n_regionkey: 0,
  n_name: "CANADA"
}
];
const customer = [
  {c_custkey: 1, c_nationkey: 10},
  {c_custkey: 2, c_nationkey: 20}
];
const orders = [
  {
  o_orderkey: 100,
  o_custkey: 1,
  o_orderdate: "1995-04-10"
},
  {
  o_orderkey: 200,
  o_custkey: 2,
  o_orderdate: "1995-07-15"
}
];
const lineitem = [
  {
  l_orderkey: 100,
  l_suppkey: 1000,
  l_partkey: 5000,
  l_extendedprice: 1000,
  l_discount: 0.1
},
  {
  l_orderkey: 200,
  l_suppkey: 2000,
  l_partkey: 5000,
  l_extendedprice: 500,
  l_discount: 0.05
}
];
const supplier = [{s_suppkey: 1000}, {s_suppkey: 2000}];
const part = [
  {
  p_partkey: 5000,
  p_type: "ECONOMY ANODIZED STEEL"
},
  {p_partkey: 6000, p_type: "SMALL BRASS"}
];
const start_date = "1995-01-01";
const end_date = "1996-12-31";
const target_type = "ECONOMY ANODIZED STEEL";
const target_nation = "BRAZIL";
const result = (() => {
  let _tmp2: Array<{ mkt_share: any; o_year: any }> = [];
  const groups = {};
  for (const l of lineitem) {
    for (const p of part) {
      if (!((p.p_partkey == l.l_partkey))) continue;
      for (const s of supplier) {
        if (!((s.s_suppkey == l.l_suppkey))) continue;
        for (const o of orders) {
          if (!((o.o_orderkey == l.l_orderkey))) continue;
          for (const c of customer) {
            if (!((c.c_custkey == o.o_custkey))) continue;
            for (const n of nation) {
              if (!((n.n_nationkey == c.c_nationkey))) continue;
              for (const r of region) {
                if (!((r.r_regionkey == n.n_regionkey))) continue;
                if (!((((((p.p_type == target_type) && (o.o_orderdate >= start_date)) && (o.o_orderdate <= end_date)) && (r.r_name == "AMERICA"))))) continue;
                const _k = JSON.stringify(o.o_orderdate.substring(0, 4));
                let year = groups[_k];
                if (!year) { year = []; year.key = o.o_orderdate.substring(0, 4); year.items = year; groups[_k] = year; }
                year.push({l: l, p: p, s: s, o: o, c: c, n: n, r: r});
              }
            }
          }
        }
      }
    }
  }
  for (const _k in groups) {
    const year = groups[_k];
    _tmp2.push({item: {
  o_year: year.key,
  mkt_share: ((year.map((x) => (() => {
  const _tmp1 = (x.n.n_name == target_nation);
  let _res;
  switch (_tmp1) {
    case true:
      _res = (x.l.l_extendedprice * ((1 - x.l.l_discount)));
      break;
    default:
      _res = 0;
      break;
  }
  return _res;
})()
).reduce((a,b)=>a+b,0)) / (year.map((x) => (x.l.l_extendedprice * ((1 - x.l.l_discount)))).reduce((a,b)=>a+b,0)))
}, key: year.key});
  }
  _tmp2 = _tmp2.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp2;
})()
;
console.log(_json(result));
const numerator = (1000 * 0.9);
const denominator = (numerator + ((500 * 0.95)));
const share = (numerator / denominator);
if (!(JSON.stringify(result) === JSON.stringify([{o_year: "1995", mkt_share: share}]))) { throw new Error("Q8 returns correct market share for BRAZIL in 1995 failed"); }
