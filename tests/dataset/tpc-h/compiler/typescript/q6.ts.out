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
  l_extendedprice: 1000,
  l_discount: 0.06,
  l_shipdate: "1994-02-15",
  l_quantity: 10
},
  {
  l_extendedprice: 500,
  l_discount: 0.07,
  l_shipdate: "1994-03-10",
  l_quantity: 23
},
  {
  l_extendedprice: 400,
  l_discount: 0.04,
  l_shipdate: "1994-04-10",
  l_quantity: 15
},
  {
  l_extendedprice: 200,
  l_discount: 0.06,
  l_shipdate: "1995-01-01",
  l_quantity: 5
}
];
const result = lineitem.filter((l) => ((((((l.l_shipdate >= "1994-01-01")) && ((l.l_shipdate < "1995-01-01"))) && ((l.l_discount >= 0.05))) && ((l.l_discount <= 0.07))) && ((l.l_quantity < 24)))).map((l) => (l.l_extendedprice * l.l_discount)).reduce((a,b)=>a+b,0);
console.log(_json(result));
if (!((result == ((((1000 * 0.06)) + ((500 * 0.07))))))) { throw new Error("Q6 calculates revenue from qualified lineitems failed"); }
