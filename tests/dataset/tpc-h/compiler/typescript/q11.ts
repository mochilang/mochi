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
  {n_nationkey: 1, n_name: "GERMANY"},
  {n_nationkey: 2, n_name: "FRANCE"}
];
const supplier = [
  {s_suppkey: 100, s_nationkey: 1},
  {s_suppkey: 200, s_nationkey: 1},
  {s_suppkey: 300, s_nationkey: 2}
];
const partsupp = [
  {
  ps_partkey: 1000,
  ps_suppkey: 100,
  ps_supplycost: 10,
  ps_availqty: 100
},
  {
  ps_partkey: 1000,
  ps_suppkey: 200,
  ps_supplycost: 20,
  ps_availqty: 50
},
  {
  ps_partkey: 2000,
  ps_suppkey: 100,
  ps_supplycost: 5,
  ps_availqty: 10
},
  {
  ps_partkey: 3000,
  ps_suppkey: 300,
  ps_supplycost: 8,
  ps_availqty: 500
}
];
const target_nation = "GERMANY";
const filtered = (() => {
  const _tmp1: Array<{ ps_partkey: any; value: any }> = [];
  for (const ps of partsupp) {
    for (const s of supplier) {
      if (!((s.s_suppkey == ps.ps_suppkey))) continue;
      for (const n of nation) {
        if (!((n.n_nationkey == s.s_nationkey))) continue;
        if (!((n.n_name == target_nation))) continue;
        _tmp1.push({
  ps_partkey: ps.ps_partkey,
  value: (ps.ps_supplycost * ps.ps_availqty)
});
      }
    }
  }
  return _tmp1;
})()
;
const grouped = (() => {
  const _tmp2: Array<{ ps_partkey: any; value: number }> = [];
  const groups = {};
  for (const x of filtered) {
    const _k = JSON.stringify(x.ps_partkey);
    let g = groups[_k];
    if (!g) { g = []; g.key = x.ps_partkey; g.items = g; groups[_k] = g; }
    g.push(x);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp2.push({
  ps_partkey: g.key,
  value: (g.map((r) => r.value).reduce((a,b)=>a+b,0))
});
  }
  return _tmp2;
})()
;
const total = (filtered.map((x) => x.value).reduce((a,b)=>a+b,0));
const threshold = (total * 0.0001);
const result = grouped.slice().sort((a,b)=> (-a.value) < (-b.value) ? -1 : (-a.value) > (-b.value) ? 1 : 0).filter((x) => (x.value > threshold));
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {ps_partkey: 1000, value: 2000},
  {ps_partkey: 2000, value: 50}
]))) { throw new Error("Q11 returns high-value partkeys from GERMANY failed"); }
