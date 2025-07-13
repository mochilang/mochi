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
const part = [
  {p_partkey: 1, p_type: "PROMO LUXURY"},
  {p_partkey: 2, p_type: "STANDARD BRASS"}
];
const lineitem = [
  {
  l_partkey: 1,
  l_extendedprice: 1000,
  l_discount: 0.1,
  l_shipdate: "1995-09-05"
},
  {
  l_partkey: 2,
  l_extendedprice: 800,
  l_discount: 0,
  l_shipdate: "1995-09-20"
},
  {
  l_partkey: 1,
  l_extendedprice: 500,
  l_discount: 0.2,
  l_shipdate: "1995-10-02"
}
];
const start_date = "1995-09-01";
const end_date = "1995-10-01";
const filtered = (() => {
  const _tmp1: Array<{ is_promo: any; revenue: any }> = [];
  for (const l of lineitem) {
    for (const p of part) {
      if (!((p.p_partkey == l.l_partkey))) continue;
      if (!(((l.l_shipdate >= start_date) && (l.l_shipdate < end_date)))) continue;
      _tmp1.push({
  is_promo: contains(p.p_type, "PROMO"),
  revenue: (l.l_extendedprice * ((1 - l.l_discount)))
});
    }
  }
  return _tmp1;
})()
;
const promo_sum = (filtered.filter((x) => x.is_promo).map((x) => x.revenue).reduce((a,b)=>a+b,0));
const total_sum = (filtered.map((x) => x.revenue).reduce((a,b)=>a+b,0));
const result = ((100 * promo_sum) / total_sum);
console.log(_json(result));
const promo = (1000 * 0.9);
const total = (900 + 800);
const expected = ((100 * promo) / total);
if (!((result == expected))) { throw new Error("Q14 calculates promo revenue percent in 1995-09 failed"); }
