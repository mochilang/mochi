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
const orders = [
  {
  o_orderkey: 1,
  o_orderpriority: "1-URGENT"
},
  {
  o_orderkey: 2,
  o_orderpriority: "3-MEDIUM"
}
];
const lineitem = [
  {
  l_orderkey: 1,
  l_shipmode: "MAIL",
  l_commitdate: "1994-02-10",
  l_receiptdate: "1994-02-15",
  l_shipdate: "1994-02-05"
},
  {
  l_orderkey: 2,
  l_shipmode: "SHIP",
  l_commitdate: "1994-03-01",
  l_receiptdate: "1994-02-28",
  l_shipdate: "1994-02-27"
}
];
const result = (() => {
  let _tmp1: Array<{ high_line_count: number; l_shipmode: any; low_line_count: number }> = [];
  const groups = {};
  for (const l of lineitem) {
    for (const o of orders) {
      if (!((o.o_orderkey == l.l_orderkey))) continue;
      if (!((((((contains(["MAIL", "SHIP"], l.l_shipmode)) && ((l.l_commitdate < l.l_receiptdate))) && ((l.l_shipdate < l.l_commitdate))) && ((l.l_receiptdate >= "1994-01-01"))) && ((l.l_receiptdate < "1995-01-01"))))) continue;
      const _k = JSON.stringify(l.l_shipmode);
      let g = groups[_k];
      if (!g) { g = []; g.key = l.l_shipmode; g.items = g; groups[_k] = g; }
      g.push({l: l, o: o});
    }
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {
  l_shipmode: g.key,
  high_line_count: (g.map((x) => (contains(["1-URGENT", "2-HIGH"], x.o.o_orderpriority) ? 1 : 0)).reduce((a,b)=>a+b,0)),
  low_line_count: (g.map((x) => ((!(contains(["1-URGENT", "2-HIGH"], x.o.o_orderpriority))) ? 1 : 0)).reduce((a,b)=>a+b,0))
}, key: g.key});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  l_shipmode: "MAIL",
  high_line_count: 1,
  low_line_count: 0
}
]))) { throw new Error("Q12 counts lineitems by ship mode and priority failed"); }
