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
const orders = [
  {
  o_orderkey: 1,
  o_orderdate: "1993-07-01",
  o_orderpriority: "1-URGENT"
},
  {
  o_orderkey: 2,
  o_orderdate: "1993-07-15",
  o_orderpriority: "2-HIGH"
},
  {
  o_orderkey: 3,
  o_orderdate: "1993-08-01",
  o_orderpriority: "3-NORMAL"
}
];
const lineitem = [
  {
  l_orderkey: 1,
  l_commitdate: "1993-07-10",
  l_receiptdate: "1993-07-12"
},
  {
  l_orderkey: 1,
  l_commitdate: "1993-07-12",
  l_receiptdate: "1993-07-10"
},
  {
  l_orderkey: 2,
  l_commitdate: "1993-07-20",
  l_receiptdate: "1993-07-25"
},
  {
  l_orderkey: 3,
  l_commitdate: "1993-08-02",
  l_receiptdate: "1993-08-01"
},
  {
  l_orderkey: 3,
  l_commitdate: "1993-08-05",
  l_receiptdate: "1993-08-10"
}
];
const start_date = "1993-07-01";
const end_date = "1993-08-01";
const date_filtered_orders = orders.filter((o) => ((o.o_orderdate >= start_date) && (o.o_orderdate < end_date)));
const late_orders = date_filtered_orders.filter((o) => lineitem.some((l) => ((l.l_orderkey == o.o_orderkey) && (l.l_commitdate < l.l_receiptdate))));
const result = (() => {
  let _tmp1: Array<{ o_orderpriority: any; order_count: number }> = [];
  const groups = {};
  for (const o of late_orders) {
    const _k = JSON.stringify(o.o_orderpriority);
    let g = groups[_k];
    if (!g) { g = []; g.key = o.o_orderpriority; g.items = g; groups[_k] = g; }
    g.push(o);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {
  o_orderpriority: g.key,
  order_count: g.length
}, key: g.key});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  o_orderpriority: "1-URGENT",
  order_count: 1
},
  {
  o_orderpriority: "2-HIGH",
  order_count: 1
}
]))) { throw new Error("Q4 returns count of orders with late lineitems in range failed"); }
