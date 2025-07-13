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
function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const nation = [
  {n_nationkey: 1, n_name: "CANADA"},
  {n_nationkey: 2, n_name: "GERMANY"}
];
const supplier = [
  {
  s_suppkey: 100,
  s_name: "Maple Supply",
  s_address: "123 Forest Lane",
  s_nationkey: 1
},
  {
  s_suppkey: 200,
  s_name: "Berlin Metals",
  s_address: "456 Iron Str",
  s_nationkey: 2
}
];
const part = [
  {
  p_partkey: 10,
  p_name: "forest glade bricks"
},
  {
  p_partkey: 20,
  p_name: "desert sand paper"
}
];
const partsupp = [
  {
  ps_partkey: 10,
  ps_suppkey: 100,
  ps_availqty: 100
},
  {
  ps_partkey: 20,
  ps_suppkey: 200,
  ps_availqty: 30
}
];
const lineitem = [
  {
  l_partkey: 10,
  l_suppkey: 100,
  l_quantity: 100,
  l_shipdate: "1994-05-15"
},
  {
  l_partkey: 10,
  l_suppkey: 100,
  l_quantity: 50,
  l_shipdate: "1995-01-01"
}
];
const prefix = "forest";
const shipped_94 = (() => {
  const _tmp1: Array<{ partkey: any; qty: number; suppkey: any }> = [];
  const groups = {};
  for (const l of lineitem) {
    if (!(((l.l_shipdate >= "1994-01-01") && (l.l_shipdate < "1995-01-01")))) continue;
    const _k = JSON.stringify({
  partkey: l.l_partkey,
  suppkey: l.l_suppkey
});
    let g = groups[_k];
    if (!g) { g = []; g.key = {
  partkey: l.l_partkey,
  suppkey: l.l_suppkey
}; g.items = g; groups[_k] = g; }
    g.push(l);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({
  partkey: g.key.partkey,
  suppkey: g.key.suppkey,
  qty: (g.map((x) => x.l_quantity).reduce((a,b)=>a+b,0))
});
  }
  return _tmp1;
})()
;
const target_partkeys = (() => {
  const _tmp2: any[] = [];
  for (const ps of partsupp) {
    for (const p of part) {
      if (!((ps.ps_partkey == p.p_partkey))) continue;
      for (const s of shipped_94) {
        if (!(((ps.ps_partkey == s.partkey) && (ps.ps_suppkey == s.suppkey)))) continue;
        if (!(((p.p_name.substring(0, prefix.length) == prefix) && (ps.ps_availqty > ((0.5 * s.qty)))))) continue;
        _tmp2.push(ps.ps_suppkey);
      }
    }
  }
  return _tmp2;
})()
;
const result = (() => {
  let _tmp3: Array<{ s_address: any; s_name: any }> = [];
  for (const s of supplier) {
    for (const n of nation) {
      if (!((n.n_nationkey == s.s_nationkey))) continue;
      if (!((contains(target_partkeys, s.s_suppkey) && (n.n_name == "CANADA")))) continue;
      _tmp3.push({item: {
  s_name: s.s_name,
  s_address: s.s_address
}, key: s.s_name});
    }
  }
  _tmp3 = _tmp3.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp3;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  s_name: "Maple Supply",
  s_address: "123 Forest Lane"
}
]))) { throw new Error("Q20 returns suppliers from CANADA with forest part stock > 50% of 1994 shipments failed"); }
