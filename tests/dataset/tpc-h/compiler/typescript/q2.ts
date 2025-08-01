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
const region = [
  {r_regionkey: 1, r_name: "EUROPE"},
  {r_regionkey: 2, r_name: "ASIA"}
];
const nation = [
  {
  n_nationkey: 10,
  n_regionkey: 1,
  n_name: "FRANCE"
},
  {
  n_nationkey: 20,
  n_regionkey: 2,
  n_name: "CHINA"
}
];
const supplier = [
  {
  s_suppkey: 100,
  s_name: "BestSupplier",
  s_address: "123 Rue",
  s_nationkey: 10,
  s_phone: "123",
  s_acctbal: 1000,
  s_comment: "Fast and reliable"
},
  {
  s_suppkey: 200,
  s_name: "AltSupplier",
  s_address: "456 Way",
  s_nationkey: 20,
  s_phone: "456",
  s_acctbal: 500,
  s_comment: "Slow"
}
];
const part = [
  {
  p_partkey: 1000,
  p_type: "LARGE BRASS",
  p_size: 15,
  p_mfgr: "M1"
},
  {
  p_partkey: 2000,
  p_type: "SMALL COPPER",
  p_size: 15,
  p_mfgr: "M2"
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
  ps_supplycost: 15
}
];
const europe_nations = (() => {
  const _tmp1: any[] = [];
  for (const r of region) {
    for (const n of nation) {
      if (!((n.n_regionkey == r.r_regionkey))) continue;
      if (!((r.r_name == "EUROPE"))) continue;
      _tmp1.push(n);
    }
  }
  return _tmp1;
})()
;
const europe_suppliers = (() => {
  const _tmp2: Array<{ n: any; s: any }> = [];
  for (const s of supplier) {
    for (const n of europe_nations) {
      if (!((s.s_nationkey == n.n_nationkey))) continue;
      _tmp2.push({s: s, n: n});
    }
  }
  return _tmp2;
})()
;
const target_parts = part.filter((p) => ((p.p_size == 15) && (p.p_type == "LARGE BRASS")));
const target_partsupp = (() => {
  const _tmp3: Array<{ n_name: any; p_mfgr: any; p_partkey: any; ps_supplycost: any; s_acctbal: any; s_address: any; s_comment: any; s_name: any; s_phone: any }> = [];
  for (const ps of partsupp) {
    for (const p of target_parts) {
      if (!((ps.ps_partkey == p.p_partkey))) continue;
      for (const s of europe_suppliers) {
        if (!((ps.ps_suppkey == s.s.s_suppkey))) continue;
        _tmp3.push({
  s_acctbal: s.s.s_acctbal,
  s_name: s.s.s_name,
  n_name: s.n.n_name,
  p_partkey: p.p_partkey,
  p_mfgr: p.p_mfgr,
  s_address: s.s.s_address,
  s_phone: s.s.s_phone,
  s_comment: s.s.s_comment,
  ps_supplycost: ps.ps_supplycost
});
      }
    }
  }
  return _tmp3;
})()
;
const costs = target_partsupp.map((x) => x.ps_supplycost);
const min_cost = Math.min(...costs);
const result = target_partsupp.slice().sort((a,b)=> (-a.s_acctbal) < (-b.s_acctbal) ? -1 : (-a.s_acctbal) > (-b.s_acctbal) ? 1 : 0).filter((x) => (x.ps_supplycost == min_cost));
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  s_acctbal: 1000,
  s_name: "BestSupplier",
  n_name: "FRANCE",
  p_partkey: 1000,
  p_mfgr: "M1",
  s_address: "123 Rue",
  s_phone: "123",
  s_comment: "Fast and reliable",
  ps_supplycost: 10
}
]))) { throw new Error("Q2 returns only supplier with min cost in Europe for brass part failed"); }
