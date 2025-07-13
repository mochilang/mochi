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
  {n_nationkey: 1, n_name: "SAUDI ARABIA"},
  {n_nationkey: 2, n_name: "FRANCE"}
];
const supplier = [
  {
  s_suppkey: 100,
  s_name: "Desert Trade",
  s_nationkey: 1
},
  {
  s_suppkey: 200,
  s_name: "Euro Goods",
  s_nationkey: 2
}
];
const orders = [
  {o_orderkey: 500, o_orderstatus: "F"},
  {o_orderkey: 600, o_orderstatus: "O"}
];
const lineitem = [
  {
  l_orderkey: 500,
  l_suppkey: 100,
  l_receiptdate: "1995-04-15",
  l_commitdate: "1995-04-10"
},
  {
  l_orderkey: 500,
  l_suppkey: 200,
  l_receiptdate: "1995-04-12",
  l_commitdate: "1995-04-12"
},
  {
  l_orderkey: 600,
  l_suppkey: 100,
  l_receiptdate: "1995-05-01",
  l_commitdate: "1995-04-25"
}
];
const result = (() => {
  let _tmp1: Array<{ numwait: number; s_name: any }> = [];
  const groups = {};
  for (const s of supplier) {
    for (const l1 of lineitem) {
      if (!((s.s_suppkey == l1.l_suppkey))) continue;
      for (const o of orders) {
        if (!((o.o_orderkey == l1.l_orderkey))) continue;
        for (const n of nation) {
          if (!((n.n_nationkey == s.s_nationkey))) continue;
          if (!(((((o.o_orderstatus == "F") && (l1.l_receiptdate > l1.l_commitdate)) && (n.n_name == "SAUDI ARABIA")) && ((!lineitem.some((x) => (((x.l_orderkey == l1.l_orderkey) && (x.l_suppkey != l1.l_suppkey)) && (x.l_receiptdate > x.l_commitdate)))))))) continue;
          const _k = JSON.stringify(s.s_name);
          let g = groups[_k];
          if (!g) { g = []; g.key = s.s_name; g.items = g; groups[_k] = g; }
          g.push({s: s, l1: l1, o: o, n: n});
        }
      }
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {s_name: g.key, numwait: g.length}, key: [(-g.length), g.key]});
  }
  _tmp1 = _tmp1.sort((a,b)=> JSON.stringify(a.key) < JSON.stringify(b.key) ? -1 : JSON.stringify(a.key) > JSON.stringify(b.key) ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([{s_name: "Desert Trade", numwait: 1}]))) { throw new Error("Q21 returns Saudi suppliers who caused unique delivery delays failed"); }
