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
const lineitem = [
  {
  l_quantity: 17,
  l_extendedprice: 1000,
  l_discount: 0.05,
  l_tax: 0.07,
  l_returnflag: "N",
  l_linestatus: "O",
  l_shipdate: "1998-08-01"
},
  {
  l_quantity: 36,
  l_extendedprice: 2000,
  l_discount: 0.1,
  l_tax: 0.05,
  l_returnflag: "N",
  l_linestatus: "O",
  l_shipdate: "1998-09-01"
},
  {
  l_quantity: 25,
  l_extendedprice: 1500,
  l_discount: 0,
  l_tax: 0.08,
  l_returnflag: "R",
  l_linestatus: "F",
  l_shipdate: "1998-09-03"
}
];
const result = (() => {
  const _tmp1: Array<{ avg_disc: number; avg_price: number; avg_qty: number; count_order: number; linestatus: any; returnflag: any; sum_base_price: number; sum_charge: number; sum_disc_price: number; sum_qty: number }> = [];
  const groups = {};
  for (const row of lineitem) {
    if (!((row.l_shipdate <= "1998-09-02"))) continue;
    const _k = JSON.stringify({
  returnflag: row.l_returnflag,
  linestatus: row.l_linestatus
});
    let g = groups[_k];
    if (!g) { g = []; g.key = {
  returnflag: row.l_returnflag,
  linestatus: row.l_linestatus
}; g.items = g; groups[_k] = g; }
    g.push(row);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({
  returnflag: g.key.returnflag,
  linestatus: g.key.linestatus,
  sum_qty: (g.map((x) => x.l_quantity).reduce((a,b)=>a+b,0)),
  sum_base_price: (g.map((x) => x.l_extendedprice).reduce((a,b)=>a+b,0)),
  sum_disc_price: (g.map((x) => (x.l_extendedprice * ((1 - x.l_discount)))).reduce((a,b)=>a+b,0)),
  sum_charge: (g.map((x) => ((x.l_extendedprice * ((1 - x.l_discount))) * ((1 + x.l_tax)))).reduce((a,b)=>a+b,0)),
  avg_qty: (g.map((x) => x.l_quantity).reduce((a,b)=>a+b,0)/g.map((x) => x.l_quantity).length),
  avg_price: (g.map((x) => x.l_extendedprice).reduce((a,b)=>a+b,0)/g.map((x) => x.l_extendedprice).length),
  avg_disc: (g.map((x) => x.l_discount).reduce((a,b)=>a+b,0)/g.map((x) => x.l_discount).length),
  count_order: g.length
});
  }
  return _tmp1;
})()
;
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  returnflag: "N",
  linestatus: "O",
  sum_qty: 53,
  sum_base_price: 3000,
  sum_disc_price: (950 + 1800),
  sum_charge: (((950 * 1.07)) + ((1800 * 1.05))),
  avg_qty: 26.5,
  avg_price: 1500,
  avg_disc: 0.07500000000000001,
  count_order: 2
}
]))) { throw new Error("Q1 aggregates revenue and quantity by returnflag + linestatus failed"); }
