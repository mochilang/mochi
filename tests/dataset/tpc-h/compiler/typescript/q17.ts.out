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
const part = [
  {
  p_partkey: 1,
  p_brand: "Brand#23",
  p_container: "MED BOX"
},
  {
  p_partkey: 2,
  p_brand: "Brand#77",
  p_container: "LG JAR"
}
];
const lineitem = [
  {
  l_partkey: 1,
  l_quantity: 1,
  l_extendedprice: 100
},
  {
  l_partkey: 1,
  l_quantity: 10,
  l_extendedprice: 1000
},
  {
  l_partkey: 1,
  l_quantity: 20,
  l_extendedprice: 2000
},
  {
  l_partkey: 2,
  l_quantity: 5,
  l_extendedprice: 500
}
];
const brand = "Brand#23";
const container = "MED BOX";
const filtered = (() => {
  const _tmp1: any[] = [];
  for (const l of lineitem) {
    for (const p of part) {
      if (!((p.p_partkey == l.l_partkey))) continue;
      if (!((((((p.p_brand == brand)) && ((p.p_container == container))) && ((l.l_quantity < ((0.2 * (lineitem.filter((x) => (x.l_partkey == p.p_partkey)).map((x) => x.l_quantity).reduce((a,b)=>a+b,0)/lineitem.filter((x) => (x.l_partkey == p.p_partkey)).map((x) => x.l_quantity).length))))))))) continue;
      _tmp1.push(l.l_extendedprice);
    }
  }
  return _tmp1;
})()
;
const result = ((filtered.reduce((a,b)=>a+b,0)) / 7);
console.log(_json(result));
const expected = (100 / 7);
if (!((result == expected))) { throw new Error("Q17 returns average yearly revenue for small-quantity orders failed"); }
