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
const customer = [
  {c_custkey: 1},
  {c_custkey: 2},
  {c_custkey: 3}
];
const orders = [
  {
  o_orderkey: 100,
  o_custkey: 1,
  o_comment: "fast delivery"
},
  {
  o_orderkey: 101,
  o_custkey: 1,
  o_comment: "no comment"
},
  {
  o_orderkey: 102,
  o_custkey: 2,
  o_comment: "special requests only"
}
];
const per_customer = customer.map((c) => ({
  c_count: orders.filter((o) => ((((o.o_custkey == c.c_custkey) && ((!(contains(o.o_comment, "special"))))) && ((!(contains(o.o_comment, "requests"))))))).length
}));
const grouped = (() => {
  let _tmp1: Array<{ c_count: any; custdist: number }> = [];
  const groups = {};
  for (const x of per_customer) {
    const _k = JSON.stringify(x.c_count);
    let g = groups[_k];
    if (!g) { g = []; g.key = x.c_count; g.items = g; groups[_k] = g; }
    g.push(x);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {c_count: g.key, custdist: g.length}, key: (-g.key)});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(_json(grouped));
if (!(JSON.stringify(grouped) === JSON.stringify([
  {c_count: 2, custdist: 1},
  {c_count: 0, custdist: 2}
]))) { throw new Error("Q13 groups customers by non-special order count failed"); }
